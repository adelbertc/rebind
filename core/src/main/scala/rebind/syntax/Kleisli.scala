package rebind
package syntax

import scalaz.{DisjunctionT, Equal, Kleisli, Monad}

trait KleisliSyntax {
  implicit def kleisliSyntax[F[_], E, A](action: DisjunctionT[F, E, A]): KleisliOps[F, E, A] =
    new KleisliOps(action)
}

class KleisliOps[F[_], E, A](val action: DisjunctionT[F, E, A]) extends AnyVal {
  import KleisliOps.KleisliAction

  private def lift(f: RetryPolicy => DisjunctionT[F, E, A]): KleisliAction[F, E, A] =
    Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A](f)

  def recover(handler: E => DisjunctionT[F, E, A])(implicit F: Monad[F]): KleisliAction[F, E, A] =
    lift(_.recover(action)(handler))

  def recoverWith(handler: PartialFunction[E, DisjunctionT[F, E, A]])(implicit F: Monad[F]): KleisliAction[F, E, A] =
    lift(_.recoverWith(action)(handler))

  def retry(limits: E => Count)(implicit E: Equal[E], F: Monad[F]): KleisliAction[F, E, A] =
    lift(_.retry(action)(limits))

  def retryAll(implicit F: Monad[F]): KleisliAction[F, E, A] =
    lift(_.retryAll(action))

  def retryConsecutive(limits: E => Count)(implicit E: Equal[E], F: Monad[F]): KleisliAction[F, E, A] =
    lift(_.retryConsecutive(action)(limits))

  def retryConsecutiveWith(limits: PartialFunction[E, Count])(implicit E: Equal[E], F: Monad[F]): KleisliAction[F, E, A] =
    lift(_.retryConsecutiveWith(action)(limits))

  def retryWith(limits: PartialFunction[E, Count])(implicit E: Equal[E], F: Monad[F]): KleisliAction[F, E, A] =
    lift(_.retryWith(action)(limits))
}

object KleisliOps {
  type KleisliAction[F[_], E, A] = Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A]
}
