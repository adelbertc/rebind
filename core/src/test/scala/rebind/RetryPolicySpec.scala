package rebind

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.specs2._
import org.specs2.time.NoTimeConversions

import scala.concurrent.duration._

import scalaz.{ Disjunction, DisjunctionT, Equal, Monad, Name, StateT }
import scalaz.scalacheck.ScalazProperties.semigroup
import scalaz.scalacheck.ScalazArbitrary.indexedStateTArb
import scalaz.std.AllInstances._

class RetryPolicySpec extends Specification with NoTimeConversions with ScalaCheck with RetryPolicySpecInstances {
  def is =
    s2"""
    capDelay                          ${capDelay}
    limitRetries                      ${limitRetries}
    iterateDelay                      ${iterateDelay}
    iterateDelay memoizes             ${iterateDelayMemoize}
    constantDelay                     ${constantDelay}
    immediate                         ${immediate}
    exponentialBackoff                ${exponentialBackoff}
    fibonaciBackoff                   ${fibonaciBackoff}

    law checking
      semigroup                       ${semigroup.laws[RetryPolicy]}

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

  val failingAction = DisjunctionT.left[Name, Oops.type, Unit](Name(Oops))

  val rightUnit = Disjunction.right(())

  val toEval = 100

  def evalPolicyMany(n: Int)(policy: RetryPolicy): Option[List[FiniteDuration]] =
    Monad[StateT[Option, policy.S, ?]].replicateM(n, policy.transition).eval(policy.initialState)

  def evalPolicyAll[A](n: Int, policy: RetryPolicy, a: A) =
    evalPolicyMany(n)(policy) must beSome((fds: List[FiniteDuration]) => fds must contain(beEqualTo(a)).forall)

  def evalPolicyExpected(n: Int, policy: RetryPolicy, expected: List[FiniteDuration]) =
    evalPolicyMany(n)(policy) must beSome((fds: List[FiniteDuration]) => fds mustEqual expected)

  type PolicyFunction[E] = RetryPolicy => DisjunctionT[Name, E, Unit] => (E => Count) => DisjunctionT[Name, E, Unit]

  def untilSuccess(policy: PolicyFunction[Oops.type]) =
    prop { (pb: PositiveByte) =>
      val action = new TestAction(pb.int, Oops, ())
      val retriedAction = policy(RetryPolicy.immediate)(action.run())(_ => Count.Infinite)
      retriedAction.run.value mustEqual rightUnit
    }

  def errorSpecificSuccess(policy: PolicyFunction[UhOh]) =
    prop { (pb: PositiveByte) =>
      val positive = pb.int

      val action = new TestAction[UhOh, Unit](positive, Uh, ())

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

      val action = new TestAction[UhOh, Unit](higher, Uh, ())

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

      val retriedAction = policy.recover(failingAction)(_ => Count.Infinite)
      retriedAction.run.value mustEqual failingAction.run.value
    }

  /* Tests */

  def capDelay =
    prop { (i: FiniteDuration, j: FiniteDuration) =>
      val lower = i.min(j)
      val higher = i.max(j)
      val policy = RetryPolicy.constantDelay(higher).capDelay(lower)

      evalPolicyAll(toEval, policy, lower)
    }

  def limitRetries =
    prop { (pb: PositiveByte) =>
      val i = pb.int
      val policy = RetryPolicy.limitRetries(i)

      val at = evalPolicyMany(i + 1)(policy)

      evalPolicyAll(i, policy, Duration.Zero) and (at must beNone)
    }

  def iterateDelay = {
    val policy = RetryPolicy.iterateDelay(1.second)(_ * 10)
    val expected = List(1.second, 10.seconds, 100.seconds, 1000.seconds, 10000.seconds)

    evalPolicyExpected(expected.size, policy, expected)
  }

  def iterateDelayMemoize =
    prop { (pb: PositiveByte, fd: FiniteDuration) =>
      val i = pb.int
      var counter = 0
      val policy = RetryPolicy.iterateDelay(fd) { fd => counter += 1; fd }
      val expected = List.fill(i)(fd)

      evalPolicyExpected(expected.size, policy, expected) and (counter mustEqual i)
    }

  def constantDelay =
    prop { (delay: FiniteDuration) =>
      val policy = RetryPolicy.constantDelay(delay)

      evalPolicyAll(toEval, policy, delay)
    }

  def immediate = evalPolicyAll(toEval, RetryPolicy.immediate, Duration.Zero)

  def exponentialBackoff = {
    val policy = RetryPolicy.exponentialBackoff(1.second)
    val expected = List(1.second, 2.seconds, 4.seconds, 8.seconds, 16.seconds)

    evalPolicyExpected(expected.size, policy, expected)
  }

  def fibonaciBackoff = {
    val policy = RetryPolicy.fibonacciBackoff(1.second)
    val expected = List(1.second, 1.second, 2.seconds, 3.seconds, 5.seconds)

    evalPolicyExpected(expected.size, policy, expected)
  }

  /* RetryPolicy#recover */

  def recoverUntilSuccess = untilSuccess(_.recover)

  def recoverErrorSpecificSuccess = errorSpecificSuccess(_.recover)

  def recoverErrorSpecificFailure = errorSpecificFailure(_.recover)

  def recoverObeyLimit =
    prop { (es: List[UhOh]) =>
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

  def recoverAllUntilSuccess =
    prop { (pb: PositiveByte) =>
      val positive = pb.int

      val action = new TestAction(positive, Oops, ())
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

  def recoverConsecutiveObeyLimit =
    prop { (pb: PositiveByte) =>
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

    retriedAction.run.value mustEqual recoveringAction.run.value
  }

  def retryingUntilSuccess =
    prop { (pb: PositiveByte) =>
      val positive = pb.int

      val action = new TestAction(positive, Oops, ())

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
  implicit val retryPolicyEqual: Equal[RetryPolicy] =
    Equal.equalBy { rp =>
      Monad[StateT[Option, rp.S, ?]].replicateM(100, rp.transition).eval(rp.initialState)
    }

  implicit val retryPolicyArbitrary: Arbitrary[RetryPolicy] =
    Arbitrary(arbitrary[(Int, StateT[Option, Int, FiniteDuration])].map {
      case (initialState, transition) => RetryPolicy.stateT(initialState)(transition)
    })
}

trait OrphanInstances {
  implicit val finiteDurationArbitrary: Arbitrary[FiniteDuration] = {
    val bound = math.pow(2, 63).toLong - 1

    Arbitrary(Gen.chooseNum(0L, bound).map(_.nanoseconds))
  }

  implicit val finiteDurationEqual: Equal[FiniteDuration] = Equal.equalA
}
