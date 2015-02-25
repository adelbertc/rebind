package rebind

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.specs2._

import scala.concurrent.duration._

import scalaz.{ Disjunction, DisjunctionT, Equal, Name }
import scalaz.scalacheck.ScalazProperties.monoid
import scalaz.std.AllInstances._

class RetryPolicySpec extends Specification with ScalaCheck with RetryPolicySpecInstances {
  def is =
    s2"""
    Law-checking:
      RetryPolicy is a lawful monoid  ${monoid.laws[RetryPolicy]}

    RetryPolicy#retrying
      retries until success           ${retryingUntilSuccess}
      exhausts policy                 ${retryingExhaustPolicy}

    RetryPolicy#recovering
      retries until success           ${recoveringUntilSuccess}
      is error-specific (success)     ${recoveringErrorSpecificSuccess}
      is error-specific (failure)     ${recoveringErrorSpecificFailure}
      obeys limits                    ${recoveringObey}
      exhausts policy                 ${recoveringExhaustPolicy}

    RetryPolicy#recoverAll
      retries until success           ${recoverAllUntilSuccess}
      exhausts policy                 ${recoverAllExhaustPolicy}
    """

  def makeFailedAction[E](n: Int, error: E): FailingAction[E] =
    new FailingAction(n, error)

  final case object Oops

  sealed abstract class UhOh
  final case object Uh extends UhOh
  final case object Oh extends UhOh

  val immediatePolicy = RetryPolicy.constantDelay(Duration.Zero)
  val failingAction = DisjunctionT.left[Name, Oops.type, Unit](Name(Oops)) // always fail
  val rightUnit = Disjunction.right(())

  /* RetryPolicy#retrying */

  // using Short because `scalaz.Name` is not stack safe
  def retryingUntilSuccess = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val action = makeFailedAction(positive, Oops)

    var counter = 0
    val retriedAction = immediatePolicy.retrying(action.run()) { _ => counter += 1; action.run() }
    (retriedAction.run.value mustEqual rightUnit) and (counter mustEqual positive)
  }

  def retryingExhaustPolicy = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val policy = RetryPolicy.limitRetries(positive)

    var counter = 0
    val retriedAction = policy.retrying(failingAction) { _ => counter += 1; failingAction }
    (retriedAction.run.value mustEqual failingAction.run.value) and (counter mustEqual positive)
  }

  /* RetryPolicy#recovering */

  def recoveringUntilSuccess = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val action = makeFailedAction(positive, Oops)

    val retriedAction = immediatePolicy.recovering(action.run())(_ => Count.Infinite)
    retriedAction.run.value mustEqual rightUnit
  }

  def recoveringErrorSpecificSuccess = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val action = makeFailedAction[UhOh](positive, Uh)

    val retriedAction =
      immediatePolicy.recovering(action.run()) {
        case Uh => Count.Finite(positive)
        case Oh => Count.Infinite
      }

    retriedAction.run.value mustEqual rightUnit
  }

  def recoveringErrorSpecificFailure =
    prop { (ipb: PositiveByte, jpb: PositiveByte) => (ipb != jpb) ==> {
      val i = ipb.byte
      val j = jpb.byte

      val lower = i.min(j).toInt - 1
      val higher = i.max(j).toInt

      val action = makeFailedAction[UhOh](higher, Uh)

      val retriedAction =
        immediatePolicy.recovering(action.run()) {
          case Uh => Count.Finite(lower)
          case Oh => Count.Infinite
        }

      retriedAction.run.value mustEqual Disjunction.left(Uh)
    }}

  def recoveringObey = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val retriedAction = immediatePolicy.recovering(failingAction)(_ => Count.Finite(positive))
    retriedAction.run.value mustEqual failingAction.run.value
  }

  def recoveringExhaustPolicy = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val policy = RetryPolicy.limitRetries(positive)

    val retriedAction = policy.recovering(failingAction)(_ => Count.Infinite)
    retriedAction.run.value mustEqual failingAction.run.value
  }

  /* RetryPolicy#recoverAll */

  def recoverAllUntilSuccess = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val action = makeFailedAction(positive, Oops)
    val retriedAction = immediatePolicy.recoverAll(action.run())
    retriedAction.run.value mustEqual rightUnit
  }

  def recoverAllExhaustPolicy = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val policy = RetryPolicy.limitRetries(positive)

    val retriedAction = policy.recoverAll(failingAction)
    retriedAction.run.value mustEqual failingAction.run.value
  }
}

trait RetryPolicySpecInstances extends OrphanInstances {
  implicit val retryPolicyEqualInstance: Equal[RetryPolicy] =
    Equal.equalBy(_.run(0))

  implicit val retryPolicyArbitraryInstance: Arbitrary[RetryPolicy] =
    Arbitrary(arbitrary[Int => Option[FiniteDuration]].map(RetryPolicy.apply))
}

trait OrphanInstances {
  implicit val finiteDurationArbitraryInstance: Arbitrary[FiniteDuration] = {
    val bound = math.pow(2, 63).toLong - 1

    Arbitrary(Gen.chooseNum(0L, bound).map(_.nanoseconds))
  }

  implicit val finiteDurationEqualInstance: Equal[FiniteDuration] =
    Equal.equal(_ == _)
}

class FailingAction[E](private var n: Int, private val error: E) {
  def run(): DisjunctionT[Name, E, Unit] =
    DisjunctionT {
      Name {
        if (n > 0) {
          n -= 1
          Disjunction.left(error)
        } else Disjunction.right(())
      }
    }
}

final case class PositiveByte(byte: Byte) extends AnyVal

object PositiveByte {
  implicit val positiveByteInstance: Arbitrary[PositiveByte] =
    Arbitrary(Gen.chooseNum[Byte](1, Byte.MaxValue).map(PositiveByte.apply))
}
