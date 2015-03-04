package rebind

import scala.concurrent.duration._

import scalaz.{ Apply, Disjunction, DisjunctionT, DLeft, DRight, Monad, Monoid }
import scalaz.std.option._
import scalaz.syntax.apply._

/** Retry policy.
  *
  * The function parameter represents the n-th retry. It should return a `Some` of a `FiniteDuration`
  * (minimum time to wait before next retry) in the case where you want to retry again, or a
  * `None` if you want to give up.
  */
final case class RetryPolicy(private[rebind] val run: Int => Option[FiniteDuration]) {
  /** Wait for a maximum of the specified time before trying again */
  def capDelay(limit: FiniteDuration): RetryPolicy =
    RetryPolicy(n => run(n).map(_.min(limit)))

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
    RetryPolicy(n => Apply[Option].apply2(this.run(n), other.run(n))(_ max _))

  /** Alias for `&&` */
  def and(other: RetryPolicy): RetryPolicy = this && other

  /** Keep retrying failures until success or the policy is exhausted.
    *
    * Stack safe so long as `F[_]` is.
    */
  def retrying[F[_] : Monad, E, A](action: DisjunctionT[F, E, A])(handler: E => DisjunctionT[F, E, A]): DisjunctionT[F, E, A] = {
    def go(newAction: DisjunctionT[F, E, A], n: Int): F[Disjunction[E, A]] =
      Monad[F].bind(newAction.run) { d =>
        val pointed = Monad[F].point(d)

        d match {
          case DLeft(e) =>
            run(n).fold(pointed) { delay =>
              Monad[F].point(DRight(Thread.sleep(delay.toMillis))) *> go(handler(e), n + 1)
            }
          case DRight(a) => pointed
        }
      }

    DisjunctionT(go(action, 0))
  }

  /** Retry with error-specific limits, or when the policy is exhausted.
    *
    * The limits indicated are compared to the total number of times the action has been retried,
    * across *all* errors. For instance, if we have:
    *
    * {{{
    * sealed abstract class UhOh
    * final case object A extends UhOh
    * final case object B extends UhOh
    *
    * somePolicy.recovering(someAction) {
    *   case A => 2.times
    *   case B => 1.time
    * }
    * }}}
    *
    * and the action first fails twice with `A` and then with `B`, the action does not retry on the `B`.
    * This is due to the fact that the action in total has been retried twice, and 2 > 1 (`B` specified bound).
    *
    * Stack safe so long as `F[_]` is.
    */
  def recovering[F[_] : Monad, E, A](action: DisjunctionT[F, E, A])(limits: E => Count): DisjunctionT[F, E, A] = {
    def go(n: Int): F[Disjunction[E, A]] =
      Monad[F].bind(action.run) { d =>
        val pointed = Monad[F].point(d)

        d match {
          case DLeft(e) =>
            run(n).filter(Function.const(limits(e) > n)).fold(pointed) { delay =>
              Monad[F].point(DRight(Thread.sleep(delay.toMillis))) *> go(n + 1)
            }
          case DRight(a) => pointed
        }
      }

    DisjunctionT(go(0))
  }

  /** Keep retrying on all errors until the policy is exhausted.
    *
    * Stack safe so long as `F[_]` is.
    */
  def recoverAll[F[_] : Monad, E, A](action: DisjunctionT[F, E, A]): DisjunctionT[F, E, A] =
    recovering(action)(Function.const(Count.Infinite))
}

object RetryPolicy extends RetryPolicyInstances with RetryPolicyFunctions

trait RetryPolicyInstances {
  implicit val retryPolicyInstance: Monoid[RetryPolicy] =
    new Monoid[RetryPolicy] {
      def append(f1: RetryPolicy, f2: => RetryPolicy): RetryPolicy = f1 && f2

      def zero: RetryPolicy = RetryPolicy(Function.const(Option(Duration.Zero)))
    }
}

trait RetryPolicyFunctions {
  /** Immediately retry the specified number of times */
  def limitRetries(i: Int): RetryPolicy =
    RetryPolicy(n => if (n < i) Option(Duration.Zero) else None)

  /** Constantly retry, starting at the specified base and iterating */
  def iterateDelay(base: FiniteDuration)(f: FiniteDuration => FiniteDuration): RetryPolicy =
    RetryPolicy(Function.const(Option(f(base))))

  /** Constantly retry, pausing a fixed amount in between */
  def constantDelay(delay: FiniteDuration): RetryPolicy =
    iterateDelay(delay)(identity)

  def immediate: RetryPolicy = constantDelay(Duration.Zero)

  /** Exponential backoff, iterating indefinitely with a seed duration */
  def exponentialBackoff(base: FiniteDuration): RetryPolicy =
    RetryPolicy(n => Option(base * math.pow(2, n.toDouble).toLong))

  /** Fibonacci backoff, iterating indefinitely with a seed duration */
  def fibonacciBackoff(base: FiniteDuration): RetryPolicy = {
    @annotation.tailrec
    def fibonacci(n: Int, state: (FiniteDuration, FiniteDuration)): FiniteDuration =
      n match {
        case 0 => state._1
        case _ => fibonacci(n - 1, (state._2, state._1 + state._2))
      }

    RetryPolicy(n => Option(fibonacci(n + 1, (Duration.Zero, base))))
  }
}
