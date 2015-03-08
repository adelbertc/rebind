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
    law checking
      monoid                          ${monoid.laws[RetryPolicy]}

    boundError
      retries until success           ${boundErrorUntilSuccess}
      is error-specific (success)     ${boundErrorErrorSpecificSuccess}
      is error-specific (failure)     ${boundErrorErrorSpecificFailure}
      obeys limits                    ${boundErrorObey}
      exhausts policy                 ${boundErrorExhaustPolicy}

    recover
      retries until success           ${recoverUntilSuccess}
      is error-specific (success)     ${recoverErrorSpecificSuccess}
      is error-specific (failure)     ${recoverErrorSpecificFailure}
      obeys limits                    ${recoverObey}
      exhausts policy                 ${recoverExhaustPolicy}

    recoverAll
      retries until success           ${recoverAllUntilSuccess}
      exhausts policy                 ${recoverAllExhaustPolicy}

    recoverConsecutive
      retries until success           ${recoverConsecutiveUntilSuccess}
      is error-specific (success)     ${recoverConsecutiveErrorSpecificSuccess}
      is error-specific (failure)     ${recoverConsecutiveErrorSpecificFailure}
      obeys limits                    ${recoverConsecutiveObey}
      exhausts policy                 ${recoverConsecutiveExhaustPolicy}

    retrying
      uses handler                    ${retryingUsesHandler}
      retries until success           ${retryingUntilSuccess}
      exhausts policy                 ${retryingExhaustPolicy}
      can iterate                     ${iterateDelay}
    """

  def makeFailedAction[E, A](n: Int, error: E, success: A): FailingAction[E, A] =
    new FailingAction(n, error, success)

  val failingAction = DisjunctionT.left[Name, Oops.type, Unit](Name(Oops)) // always fail
  val rightUnit = Disjunction.right(())

  /* RetryPolicy#boundError */

  // using Byte because `scalaz.Name` is not stack safe
  def boundErrorUntilSuccess = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val action = makeFailedAction(positive, Oops, ())

    val retriedAction = RetryPolicy.immediate.boundError(action.run())(_ => Count.Infinite)
    retriedAction.run.value mustEqual rightUnit
  }

  def boundErrorErrorSpecificSuccess = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val action = makeFailedAction[UhOh, Unit](positive, Uh, ())

    val retriedAction =
      RetryPolicy.immediate.boundError(action.run()) {
        case Uh => Count.Finite(positive)
        case Oh => Count.Infinite
      }

    retriedAction.run.value mustEqual rightUnit
  }

  def boundErrorErrorSpecificFailure =
    prop { (ipb: PositiveByte, jpb: PositiveByte) => (ipb != jpb) ==> {
      val i = ipb.byte
      val j = jpb.byte

      val lower = i.min(j).toInt - 1
      val higher = i.max(j).toInt

      val action = makeFailedAction[UhOh, Unit](higher, Uh, ())

      val retriedAction =
        RetryPolicy.immediate.boundError(action.run()) {
          case Uh => Count.Finite(lower)
          case Oh => Count.Infinite
        }

      retriedAction.run.value mustEqual Disjunction.left(Uh)
    }}

  def boundErrorObey = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val retriedAction = RetryPolicy.immediate.boundError(failingAction)(_ => Count.Finite(positive))
    retriedAction.run.value mustEqual failingAction.run.value
  }

  def boundErrorExhaustPolicy = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val policy = RetryPolicy.limitRetries(positive)

    val retriedAction = policy.boundError(failingAction)(_ => Count.Infinite)
    retriedAction.run.value mustEqual failingAction.run.value
  }

  /* RetryPolicy#recover */

  def recoverUntilSuccess = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val action = makeFailedAction(positive, Oops, ())

    val retriedAction = RetryPolicy.immediate.recover(action.run())(_ => Count.Infinite)
    retriedAction.run.value mustEqual rightUnit
  }

  def recoverErrorSpecificSuccess = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val action = makeFailedAction[UhOh, Unit](positive, Uh, ())

    val retriedAction =
      RetryPolicy.immediate.recover(action.run()) {
        case Uh => Count.Finite(positive)
        case Oh => Count.Infinite
      }

    retriedAction.run.value mustEqual rightUnit
  }

  def recoverErrorSpecificFailure =
    prop { (ipb: PositiveByte, jpb: PositiveByte) => (ipb != jpb) ==> {
      val i = ipb.byte
      val j = jpb.byte

      val lower = i.min(j).toInt - 1
      val higher = i.max(j).toInt

      val action = makeFailedAction[UhOh, Unit](higher, Uh, ())

      val retriedAction =
        RetryPolicy.immediate.recover(action.run()) {
          case Uh => Count.Finite(lower)
          case Oh => Count.Infinite
        }

      retriedAction.run.value mustEqual Disjunction.left(Uh)
    }}

  def recoverObey = prop { (i : Byte) =>
    val positive = (i + 1).abs

    val retriedAction = RetryPolicy.immediate.recover(failingAction)(_ => Count.Finite(positive))
    retriedAction.run.value mustEqual failingAction.run.value
  }

  def recoverExhaustPolicy = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val policy = RetryPolicy.limitRetries(positive)

    val retriedAction = policy.recover(failingAction)(_ => Count.Infinite)
    retriedAction.run.value mustEqual failingAction.run.value
  }

  /* RetryPolicy#recoverAll */

  def recoverAllUntilSuccess = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val action = makeFailedAction(positive, Oops, ())
    val retriedAction = RetryPolicy.immediate.recoverAll(action.run())
    retriedAction.run.value mustEqual rightUnit
  }

  def recoverAllExhaustPolicy = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val policy = RetryPolicy.limitRetries(positive)

    val retriedAction = policy.recoverAll(failingAction)
    retriedAction.run.value mustEqual failingAction.run.value
  }

  /* RetryPolicy#recoverConsecutive */

  def recoverConsecutiveUntilSuccess = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val action = makeFailedAction(positive, Oops, ())

    val retriedAction = RetryPolicy.immediate.recoverConsecutive(action.run())(_ => Count.Infinite)
    retriedAction.run.value mustEqual rightUnit
  }

  def recoverConsecutiveErrorSpecificSuccess = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val action = makeFailedAction[UhOh, Unit](positive, Uh, ())

    val retriedAction =
      RetryPolicy.immediate.recoverConsecutive(action.run()) {
        case Uh => Count.Finite(positive)
        case Oh => Count.Infinite
      }

    retriedAction.run.value mustEqual rightUnit
  }

  def recoverConsecutiveErrorSpecificFailure =
    prop { (ipb: PositiveByte, jpb: PositiveByte) => (ipb != jpb) ==> {
      val i = ipb.byte
      val j = jpb.byte

      val lower = i.min(j).toInt - 1
      val higher = i.max(j).toInt

      val action = makeFailedAction[UhOh, Unit](higher, Uh, ())

      val retriedAction =
        RetryPolicy.immediate.recoverConsecutive(action.run()) {
          case Uh => Count.Finite(lower)
          case Oh => Count.Infinite
        }

      retriedAction.run.value mustEqual Disjunction.left(Uh)
    }}

  def recoverConsecutiveObey = prop { (i : Byte) =>
    val positive = (i + 1).abs

    val retriedAction = RetryPolicy.immediate.recoverConsecutive(failingAction)(_ => Count.Finite(positive))
    retriedAction.run.value mustEqual failingAction.run.value
  }

  def recoverConsecutiveExhaustPolicy = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val policy = RetryPolicy.limitRetries(positive)

    val retriedAction = policy.recoverConsecutive(failingAction)(_ => Count.Infinite)
    retriedAction.run.value mustEqual failingAction.run.value
  }

  /* RetryPolicy#retrying */

  def retryingUsesHandler = {
    val failWithUhAction = DisjunctionT.left[Name, UhOh, String](Name(Uh))

    val recoverString = "recovered"
    val recoveringAction = DisjunctionT.right[Name, UhOh, String](Name(recoverString))

    val shouldNotBeString = "should not happen"
    val shouldNotBeAction = DisjunctionT.right[Name, UhOh, String](Name(shouldNotBeString))

    val retriedAction =
      RetryPolicy.immediate.retrying(failWithUhAction) {
        case Uh => recoveringAction
        case Oh => shouldNotBeAction
      }

    retriedAction.run.value mustEqual Disjunction.right(recoverString)
  }

  def retryingUntilSuccess = prop { (i: Byte) =>
    val int = i.toInt + 1
    val positive = int.abs

    val action = makeFailedAction(positive, Oops, ())

    var counter = 0
    val retriedAction = RetryPolicy.immediate.retrying(action.run()) { _ => counter += 1; action.run() }
    (retriedAction.run.value mustEqual rightUnit) and (counter mustEqual positive)
  }

  def retryingExhaustPolicy = prop { (i: Byte) =>
    val positive = (i + 1).abs

    val policy = RetryPolicy.limitRetries(positive)

    var counter = 0
    val retriedAction = policy.retrying(failingAction) { _ => counter += 1; failingAction }
    (retriedAction.run.value mustEqual failingAction.run.value) and (counter mustEqual positive)
  }

  def iterateDelay = {
    val policy = RetryPolicy.iterateDelay(1.second)(_ * 2)

    policy.run(0) mustEqual Some(1.second)
    policy.run(1) mustEqual Some(2.seconds)
    policy.run(2) mustEqual Some(4.seconds)
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

  implicit val finiteDurationEqualInstance: Equal[FiniteDuration] = Equal.equalA
}

class FailingAction[E, A](private var n: Int, private val error: E, private val success: A) {
  def run(): DisjunctionT[Name, E, A] =
    DisjunctionT {
      Name {
        if (n > 0) {
          n -= 1
          Disjunction.left(error)
        } else Disjunction.right(success)
      }
    }
}

final case class PositiveByte(byte: Byte) extends AnyVal

object PositiveByte {
  implicit val positiveByteInstance: Arbitrary[PositiveByte] =
    Arbitrary(Gen.chooseNum[Byte](1, Byte.MaxValue).map(PositiveByte.apply))
}

final case object Oops {
  implicit val oopsInstance: Equal[Oops.type] = Equal.equalA
}

sealed abstract class UhOh
final case object Uh extends UhOh
final case object Oh extends UhOh

object UhOh {
  implicit val uhOhInstance: Equal[UhOh] = Equal.equalA
}
