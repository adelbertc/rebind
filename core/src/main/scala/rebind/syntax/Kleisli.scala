package rebind
package syntax

import scalaz.{ DisjunctionT, Equal, Kleisli, Monad }

trait KleisliSyntax {
  implicit def kleisliSyntax[F[_], E, A](action: DisjunctionT[F, E, A]): KleisliOps[F, E, A] =
    new KleisliOps(action)
}

class KleisliOps[F[_], E, A](val action: DisjunctionT[F, E, A]) extends AnyVal {
  private def lift(f: RetryPolicy => DisjunctionT[F, E, A]): Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A] =
    Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A](f)

  def recover(limits: E => Count)(implicit E: Equal[E], F: Monad[F]): Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A] =
    lift(_.recover(action)(limits))

  def recoverAll(implicit F: Monad[F]): Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A] =
    lift(_.recoverAll(action))

  def recoverConsecutive(limits: E => Count)(implicit E: Equal[E], F: Monad[F]): Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A] =
    lift(_.recoverConsecutive(action)(limits))

  def retrying(handler: E => DisjunctionT[F, E, A])(implicit F: Monad[F]): Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A] =
    lift(_.retrying(action)(handler))
}
