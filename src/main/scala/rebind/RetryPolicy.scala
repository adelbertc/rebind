package rebind

import scala.concurrent.duration._

import scalaz.{ DisjunctionT, Monad, Monoid }

/** Retry policy.
  *
  * The function parameter represents the n-th retry. It should return a `Some` of a `FiniteDuration`
  * (minimum time to wait before next retry) in the case where you want to retry again, or a
  * `None` if you want to give up.
  */
final case class RetryPolicy(run: Int => Option[FiniteDuration]) extends AnyVal {
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
    RetryPolicy { n =>
      for {
        a <- this.run(n)
        b <- other.run(n)
      } yield a.max(b)
    }

  /** Alias for `&&` */
  def and(other: RetryPolicy): RetryPolicy = this && other

  /** Keep retrying failures until success or the policy is exhausted */
  def retrying[F[_] : Monad, E, A](action: DisjunctionT[F, E, A])(handler: E => DisjunctionT[F, E, A]): DisjunctionT[F, E, A] = {
    def go(n: Int): DisjunctionT[F, E, A] =
      for {
        b <-  DisjunctionT.right(action.isLeft)
        r <-  if (b) run(n).fold(action) { delay =>
                for {
                  _ <- DisjunctionT.right(Monad[F].point(Thread.sleep(delay.toMillis)))
                  s <- go(n + 1)
                } yield s
              } else action
      } yield r

    go(0)
  }

  /** Retry failures a different number of times depending on the error, or halt when the policy is exhausted */
  def recovering[F[_] : Monad, E, A](action: DisjunctionT[F, E, A])(handler: E => Count): DisjunctionT[F, E, A] = {
    val swappedAction = action.swap

    def go(n: Int): DisjunctionT[F, A, E] =
      for {
        e <-  swappedAction
        r <-  run(n).filter(Function.const(handler(e) >= n)).fold(swappedAction) { delay =>
                for {
                  _ <- DisjunctionT.right(Monad[F].point(Thread.sleep(delay.toMillis)))
                  r <- go(n + 1)
                } yield r
              }
      } yield r
    go(0).swap
  }

  /** Keep retrying on all errors until the policy is exhausted */
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

  /** Infinitely retry, starting at the specified base and iterating */
  def alwaysWith(base: FiniteDuration)(f: FiniteDuration => FiniteDuration): RetryPolicy =
    RetryPolicy(Function.const(Option(f(base))))

  /** Constantly retry, pausing a fixed amount in between */
  def constantDelay(delay: FiniteDuration): RetryPolicy =
    alwaysWith(delay)(identity)

  /** Exponential backoff, iterating indefinitely with a seed duration */
  def exponentialBackoff(base: FiniteDuration): RetryPolicy =
    RetryPolicy(n => Option(base * math.pow(2, n.toDouble).toLong))

  /** Fibonacci backoff, iterating indefinitely with a seed duration */
  def fibonacciBackoff(base: FiniteDuration): RetryPolicy = {
    def fibonacci(n: Int, state: (FiniteDuration, FiniteDuration)): FiniteDuration =
      n match {
        case 0 => state._1
        case _ => fibonacci(n - 1, (state._2, state._1 + state._2))
      }

    RetryPolicy(n => Option(fibonacci(n + 1, (Duration.Zero, base))))
  }
}
