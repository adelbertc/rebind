package rebind

import scala.concurrent.duration._
import scala.util.Random

import scalaz.{Apply, Disjunction, DisjunctionT, DLeft, DRight, Equal, Foldable, IList, Monad, Semigroup, StateT, Zipper}
import scalaz.std.option._
import scalaz.syntax.apply._

sealed abstract class RetryPolicy { outer =>
  private[rebind] type S

  private[rebind] def initialState: S

  private[rebind] def transition: StateT[Option, S, FiniteDuration]

  /** Wait for a maximum of the specified time before trying again */
  def capDelay(limit: FiniteDuration): RetryPolicy =
    new RetryPolicy {
      type S = outer.S

      def initialState = outer.initialState

      def transition: StateT[Option, S, FiniteDuration] = outer.transition.map(_.min(limit))
    }

  /** Combine this policy with another.
    *
    * If either policy decides to stop retrying, then so will the resultant one.
    *
    * If both policies want to retry, the one with the greater delay will be used.
    *
    * Example:
    * {{{
    * // Exponential backoff starting with 1 second, up to 5 times
    * RetryPolicy.exponentialBackoff(1.second) && RetryPolicy.limitRetries(5)
    * }}}
    */
  def &&(other: RetryPolicy): RetryPolicy =
    new RetryPolicy {
      type S = (outer.S, other.S)

      def initialState = (outer.initialState, other.initialState)

      def transition: StateT[Option, (outer.S, other.S), FiniteDuration] =
        StateT {
          case (outerState, otherState) =>
            Apply[Option].apply2(outer.transition(outerState), other.transition(otherState)) {
              case ((outerNext, outerVal), (otherNext, otherVal)) =>
                ((outerNext, otherNext), outerVal.max(otherVal))
            }
        }
    }

  /** Alias for `&&` */
  def and(other: RetryPolicy): RetryPolicy = this && other

  /** Keep trying to recover until success or the policy is exhausted. */
  def recover[F[_] : Monad, E, A](action: DisjunctionT[F, E, A])(handler: E => DisjunctionT[F, E, A]): DisjunctionT[F, E, A] =
    unfold(action.run, ())(e => handler(e).run, (_, _) => Option(()))

  /** Keep trying to recover until success or the policy is exhausted.
    *
    * Can retry with a different action on certain errors - unspecified errors will retry same action.
    */
  def recoverWith[F[_] : Monad, E, A](action: DisjunctionT[F, E, A])(
                                      handler: PartialFunction[E, DisjunctionT[F, E, A]]): DisjunctionT[F, E, A] =
    recover(action)(e => if (handler.isDefinedAt(e)) handler(e) else action)

  /** Retry with error-specific limits, or when policy is exhausted.
    *
    * Limits are compared against the total number of times the error has occured so far,
    * regardless of when they occured (e.g. occured non-consecutively).
    */
  def retry[F[_] : Monad, E : Equal, A](action: DisjunctionT[F, E, A])(limits: E => Count): DisjunctionT[F, E, A] = {
    def checkError(error: E, history: IList[(E, Int)]): Option[IList[(E, Int)]] =
      limits(error) match {
        case Count.Finite(0) => None
        case _ =>
          val zipper = history.toZipper.flatMap { z =>
            if (Equal[E].equal(z.focus._1, error)) Option(z)
            else z.findNext(p => Equal[E].equal(p._1, error))
          }

          zipper match {
            case None => Option((error, 1) :: history)
            case Some(z) =>
              val newCount = z.focus._2 + 1
              if (limits(error) >= newCount) {
                val updatedZipper = z.modify { case (e, _) => (e, newCount) }
                Option(Foldable[Zipper].foldRight(updatedZipper, IList.empty[(E, Int)])(_ :: _))
              } else None
          }
      }

    val unwrapped = action.run

    unfold(unwrapped, IList.empty[(E, Int)])(Function.const(unwrapped), checkError)
  }

  /** Keep retrying on all errors until the policy is exhausted. */
  def retryAll[F[_] : Monad, E, A](action: DisjunctionT[F, E, A]): DisjunctionT[F, E, A] =
    recover(action)(Function.const(action))

  /** Retry with error-specific limits on consecutive errors, or when policy is exhausted.
    *
    * Limits are compared against consecutive occurences. For instance, if a particular error
    * is mapped to `5.times` and so far it has failed consecutively 4 times but then fails with
    * a different error, the count is reset.
    */
  def retryConsecutive[F[_] : Monad, E : Equal, A](action: DisjunctionT[F, E, A])(limits: E => Count): DisjunctionT[F, E, A] = {
    def checkError(error: E, count: Option[(E, Int)]): Option[(Option[(E, Int)])] =
      count match {
        // first iteration
        case None =>
          if (limits(error) > 0) Option(Option((error, 1)))
          else None

        // same error as last iteration
        case Some((e, n)) if Equal[E].equal(e, error) =>
          val newCount = n + 1
          if (limits(error) >= newCount) Option(Option((e, newCount)))
          else None

        // different error as last iteration
        case Some((e, _)) =>
          if (limits(error) > 0) Option(Option((error, 1)))
          else None
      }

    val unwrapped = action.run

    unfold(unwrapped, (Option.empty[(E, Int)]))(Function.const(unwrapped), checkError)
  }

  /** Retry certain errors on consecutive errors, or when policy is exhausted.
    *
    * Limits are compared against consecutive occurences. For instance, if a particular error
    * is mapped to `5.times` and so far it has failed consecutively 4 times but then fails with
    * a different error, the count is reset.
    */
  def retryConsecutiveWith[F[_] : Monad, E : Equal, A](action: DisjunctionT[F, E, A])(limits: PartialFunction[E, Count]): DisjunctionT[F, E, A] = {
    val lifted = limits.lift
    retryConsecutive(action) { e =>
      lifted(e).fold(Count.finite(0))(identity)
    }
  }

  /** Retry certain errors up to a limit, or when policy is exhausted.
    *
    * Limits are compared against the total number of times the error has occured so far,
    * regardless of when they occured (e.g. occured non-consecutively).
    */
  def retryWith[F[_] : Monad, E : Equal, A](action: DisjunctionT[F, E, A])(limits: PartialFunction[E, Count]): DisjunctionT[F, E, A] = {
    val lifted = limits.lift
    retry(action) { e =>
      lifted(e).fold(Count.finite(0))(identity)
    }
  }

  private def unfold[F[_] : Monad, E, A, T](currentAction: F[Disjunction[E, A]], initialTest: T)(
                                            next: E => F[Disjunction[E, A]],
                                            test: (E, T) => Option[T]): DisjunctionT[F, E, A] = {
    def go(action: F[Disjunction[E, A]], nextState: S, nextTest: T): F[Disjunction[E, A]] =
      Monad[F].bind(action) { d =>
        val pointed = Monad[F].point(d)

        d match {
          case DLeft(e) =>
            Apply[Option].tuple2(transition(nextState), test(e, nextTest)).fold(pointed) {
              case ((anotherState, delay), anotherTest) =>
                Monad[F].point(DRight(Thread.sleep(delay.toMillis))) *> go(next(e), anotherState, anotherTest)
            }
          case DRight(_) => pointed
        }
      }

    DisjunctionT(go(currentAction, initialState, initialTest))
  }

}

object RetryPolicy extends RetryPolicyInstances with RetryPolicyFunctions {
  /** Create a retry policy with a state transition function.
    *
    * Iterates with `next` starting with `initial`. `next` should return a `Some` of
    * a pair of `S` (the next state) and `FiniteDuration` (minimum time to wait before
    * next retry) if you want to retry again, or a `None` if you want to give up.
    */
  def apply[S](initial: S)(next: S => Option[(S, FiniteDuration)]): RetryPolicy =
    stateT(initial)(StateT(next))

  /** Create a retry policy with a state transition function.
    *
    * Iterates with `next` starting with `initial`. `next` should return a `Some` of
    * a pair of `S` (the next state) and `FiniteDuration` (minimum time to wait before
    * next retry) if you want to retry again, or a `None` if you want to give up.
    */
  def stateT[T](initial: T)(next: StateT[Option, T, FiniteDuration]): RetryPolicy =
    new RetryPolicy {
      type S = T

      def initialState = initial

      def transition = next
    }
}

trait RetryPolicyInstances {
  implicit val retryPolicyInstance: Semigroup[RetryPolicy] =
    Semigroup.instance(_ && _)
}

trait RetryPolicyFunctions {
  /** Constantly retry, pausing a fixed amount in between */
  def constantDelay(delay: FiniteDuration): RetryPolicy =
    RetryPolicy(())(Function.const(Option(((), delay))))

  /** Exponential backoff, iterating indefinitely with a seed duration */
  def exponentialBackoff(base: FiniteDuration): RetryPolicy =
    RetryPolicy(1L)(n => Option((2 * n, base * n)))

  /** Fibonacci backoff, iterating indefinitely with a seed duration */
  def fibonacciBackoff(base: FiniteDuration): RetryPolicy =
    RetryPolicy((base, base)) {
      case (next, after) =>
        val nextState = (after, next + after)
        Option((nextState, next))
    }

  /** Constantly retry immediately */
  def immediate: RetryPolicy = constantDelay(Duration.Zero)

  /** Constantly retry, starting at the specified base and iterating */
  def iterateDelay(base: FiniteDuration)(f: FiniteDuration => FiniteDuration): RetryPolicy =
    RetryPolicy(base)(fd => Option((f(fd), fd)))

  /** Immediately retry the specified number of times */
  def limitRetries(i: Int): RetryPolicy =
    RetryPolicy(0)(n => if (n < i) Option((n + 1, Duration.Zero)) else None)

  /** Constantly retry, pausing for pivot +/- epsilon. */
  def random(pivot: FiniteDuration, epsilon: FiniteDuration): RetryPolicy = {
    def random(): FiniteDuration = {
      val randomDuration = (Random.nextLong() % epsilon.toNanos).nanoseconds
      if (Random.nextBoolean()) pivot + randomDuration else pivot - randomDuration
    }

    iterateDelay(random())(_ => random())
  }
}
