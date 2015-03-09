package rebind

import scala.concurrent.duration._

import scalaz.{ Apply, Disjunction, DisjunctionT, DLeft, DRight, Equal, Foldable, IList, Monad, Monoid, Zipper }
import scalaz.std.anyVal.intInstance
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

  /** Retry with error-specific limits on total number of errors, or when the policy is exhausted.
    *
    * The limits indicated are compared to the total number of times the action has been retried,
    * across *all* errors. For instance, if we have:
    *
    * {{{
    * sealed abstract class UhOh
    * final case object A extends UhOh
    * final case object B extends UhOh
    *
    * somePolicy.boundError(someAction) {
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
  def boundError[F[_] : Monad, E, A](action: DisjunctionT[F, E, A])(limits: E => Count): DisjunctionT[F, E, A] = {
    val unwrapped = action.run
    unfold(unwrapped, ())(Function.const(unwrapped), (e, _, n) => if (limits(e) > n) Some(()) else None)
  }

  /** Retry with error-specific limits, or when policy is exhausted.
    *
    * Limits are compared against the total number of times the error has occured so far,
    * regardless of when they occured (e.g. occured non-consecutively).
    */
  def recover[F[_] : Monad, E : Equal, A](action: DisjunctionT[F, E, A])(limits: E => Count): DisjunctionT[F, E, A] = {
    def checkError(error: E, history: IList[(E, Int)], iteration: Int): Option[IList[(E, Int)]] =
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

  /** Keep retrying on all errors until the policy is exhausted.
    *
    * Stack safe so long as `F[_]` is.
    */
  def recoverAll[F[_] : Monad, E, A](action: DisjunctionT[F, E, A]): DisjunctionT[F, E, A] =
    boundError(action)(Function.const(Count.Infinite))

  /** Retry with error-specific limits on consecutive errors, or when policy is exhausted.
    *
    * Limits are compared against consecutive occurences. For instance, if a particular error
    * is mapped to `5.times` and so far it has failed consecutively 4 times but then fails with
    * a different error, the count is reset.
    */
  def recoverConsecutive[F[_] : Monad, E : Equal, A](action: DisjunctionT[F, E, A])(limits: E => Count): DisjunctionT[F, E, A] = {
    def checkError(error: E, count: Option[(E, Int)], iteration: Int): Option[(Option[(E, Int)])] =
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

  /** Keep retrying failures until success or the policy is exhausted.
    *
    * Stack safe so long as `F[_]` is.
    */
  def retrying[F[_] : Monad, E, A](action: DisjunctionT[F, E, A])(handler: E => DisjunctionT[F, E, A]): DisjunctionT[F, E, A] =
    unfold(action.run, ())(e => handler(e).run, (_, _, _) => Option(()))

  private def unfold[F[_] : Monad, E, A, S](currentAction: F[Disjunction[E, A]], firstState: S)(
                                            next: E => F[Disjunction[E, A]],
                                            p: (E, S, Int) => Option[S]): DisjunctionT[F, E, A] = {
    def go(action: F[Disjunction[E, A]], n: Int, state: S): F[Disjunction[E, A]] =
      Monad[F].bind(action) { d =>
        val pointed = Monad[F].point(d)

        d match {
          case DLeft(e) =>
            Apply[Option].tuple2(run(n), p(e, state, n)).fold(pointed) {
              case (delay, nextState) =>
                Monad[F].point(DRight(Thread.sleep(delay.toMillis))) *> go(next(e), n + 1, nextState)
            }
          case DRight(_) => pointed
        }
      }

    DisjunctionT(go(currentAction, 0, firstState))
  }

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
