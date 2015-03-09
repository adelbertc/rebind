package rebind

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.specs2._

import scala.concurrent.duration._

import scalaz.{ Disjunction, DisjunctionT, Equal, Monad, Name }
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
      obeys limits                    ${boundErrorObeyLimit}
      exhausts policy                 ${boundErrorExhaustPolicy}

    recover
      retries until success           ${recoverUntilSuccess}
      is error-specific (success)     ${recoverErrorSpecificSuccess}
      is error-specific (failure)     ${recoverErrorSpecificFailure}
      obeys limits                    ${recoverObeyLimit}
      exhausts policy                 ${recoverExhaustPolicy}

    recoverAll
      retries until success           ${recoverAllUntilSuccess}
      exhausts policy                 ${recoverAllExhaustPolicy}

    recoverConsecutive
      retries until success           ${recoverConsecutiveUntilSuccess}
      is error-specific (success)     ${recoverConsecutiveErrorSpecificSuccess}
      is error-specific (failure)     ${recoverConsecutiveErrorSpecificFailure}
      obeys limits                    ${recoverConsecutiveObeyLimit}
      exhausts policy                 ${recoverConsecutiveExhaustPolicy}

    retrying
      uses handler                    ${retryingUsesHandler}
      retries until success           ${retryingUntilSuccess}
      exhausts policy                 ${retryingExhaustPolicy}
    """

  def makeFailedAction[E, A](n: Int, error: E, success: A): TestAction[E, A] =
    new TestAction(n, error, success)

  val failingAction = DisjunctionT.left[Name, Oops.type, Unit](Name(Oops)) // always fail
  val rightUnit = Disjunction.right(())

  type PolicyFunction[E] = RetryPolicy => DisjunctionT[Name, E, Unit] => (E => Count) => DisjunctionT[Name, E, Unit]

  def untilSuccess(policy: PolicyFunction[Oops.type]) =
    prop { (pb: PositiveByte) =>
      val action = makeFailedAction(pb.int, Oops, ())
      val retriedAction = policy(RetryPolicy.immediate)(action.run())(_ => Count.Infinite)
      retriedAction.run.value mustEqual rightUnit
    }

  def errorSpecificSuccess(policy: PolicyFunction[UhOh]) =
    prop { (pb: PositiveByte) =>
      val positive = pb.int

      val action = makeFailedAction[UhOh, Unit](positive, Uh, ())

      val retriedAction =
        policy(RetryPolicy.immediate)(action.run()) {
          case Uh => Count.Finite(positive)
          case Oh => Count.Infinite
        }

      retriedAction.run.value mustEqual rightUnit
    }

  def errorSpecificFailure(policy: PolicyFunction[UhOh]) =
    prop { (ipb: PositiveByte, jpb: PositiveByte) => (ipb != jpb) ==> {
      val i = ipb.int
      val j = jpb.int

      val lower = i.min(j)
      val higher = i.max(j)

      val action = makeFailedAction[UhOh, Unit](higher, Uh, ())

      val retriedAction =
        policy(RetryPolicy.immediate)(action.run()) {
          case Uh => Count.Finite(lower)
          case Oh => Count.Infinite
        }

      retriedAction.run.value mustEqual Disjunction.left(Uh)
    }}

  def exhaustPolicy(policy: PolicyFunction[Oops.type]) =
    prop { (pb: PositiveByte) =>
      val positive = pb.int

      val policy = RetryPolicy.limitRetries(positive)

      val retriedAction = policy.boundError(failingAction)(_ => Count.Infinite)
      retriedAction.run.value mustEqual failingAction.run.value
    }

  /* RetryPolicy#boundError */

  def boundErrorUntilSuccess = untilSuccess(_.boundError)

  def boundErrorErrorSpecificSuccess = errorSpecificSuccess(_.boundError)

  def boundErrorErrorSpecificFailure = errorSpecificFailure(_.boundError)

  def boundErrorObeyLimit = prop { (pb: PositiveByte) =>
    val retriedAction = RetryPolicy.immediate.boundError(failingAction)(_ => Count.Finite(pb.int))
    retriedAction.run.value mustEqual failingAction.run.value
  }

  def boundErrorExhaustPolicy = exhaustPolicy(_.boundError)

  /* RetryPolicy#recover */

  def recoverUntilSuccess = untilSuccess(_.recover)

  def recoverErrorSpecificSuccess = errorSpecificSuccess(_.recover)

  def recoverErrorSpecificFailure = errorSpecificFailure(_.recover)

  def recoverObeyLimit = prop { (es: List[UhOh]) =>
    val limited = es.take(Byte.MaxValue)
    val numberOfOhs = limited.count(Equal[UhOh].equal(Uh, _)) + 1
    val stream = limited.toStream ++ Stream(Uh) ++ Stream.continually(Oh)

    val action = new TestAction(stream, ())
    val retriedAction =
      RetryPolicy.immediate.recover(action.run()) {
        case Uh => Count.Finite(numberOfOhs - 1)
        case Oh => Count.Infinite
      }

    retriedAction.run.value mustEqual Disjunction.left(Uh)
  }

  def recoverExhaustPolicy = exhaustPolicy(_.recover)

  /* RetryPolicy#recoverAll */

  def recoverAllUntilSuccess = prop { (pb: PositiveByte) =>
    val positive = pb.int

    val action = makeFailedAction(positive, Oops, ())
    val retriedAction = RetryPolicy.immediate.recoverAll(action.run())
    retriedAction.run.value mustEqual rightUnit
  }

  def recoverAllExhaustPolicy = {
    val function: PolicyFunction[Oops.type] = policy => action => m => policy.recoverAll(action)
    exhaustPolicy(function)
  }

  /* RetryPolicy#recoverConsecutive */

  def recoverConsecutiveUntilSuccess = untilSuccess(_.recoverConsecutive)

  def recoverConsecutiveErrorSpecificSuccess = errorSpecificSuccess(_.recoverConsecutive)

  def recoverConsecutiveErrorSpecificFailure = errorSpecificFailure(_.recoverConsecutive)

  def recoverConsecutiveObeyLimit = prop { (pb: PositiveByte) =>
    val i = pb.int
    val first = i - 1
    val last = i
    val errors: Stream[UhOh] = Stream.fill(first)(Uh) ++ Stream(Oh) ++ Stream.fill(last + 1)(Uh)
    val action = new TestAction(errors, ())
    val retriedAction =
      RetryPolicy.immediate.recoverConsecutive(action.run()) {
        case Uh => Count.Finite(last)
        case Oh => Count.Infinite
      }

    (retriedAction.run.value mustEqual Disjunction.left(Uh)) and (action.run().run.value mustEqual rightUnit)
  }

  def recoverConsecutiveExhaustPolicy = exhaustPolicy(_.recoverConsecutive)

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

  def retryingUntilSuccess = prop { (pb: PositiveByte) =>
    val positive = pb.int

    val action = makeFailedAction(positive, Oops, ())

    var counter = 0
    val retriedAction = RetryPolicy.immediate.retrying(action.run()) { _ => counter += 1; action.run() }
    (retriedAction.run.value mustEqual rightUnit) and (counter mustEqual positive)
  }

  def retryingExhaustPolicy = {
    val function: PolicyFunction[Oops.type] = policy => action => m =>
      policy.retrying(action)(_ => action)

    exhaustPolicy(function)
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
