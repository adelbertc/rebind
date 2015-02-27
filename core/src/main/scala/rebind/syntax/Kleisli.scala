package rebind
package syntax

import scalaz.{ DisjunctionT, Kleisli, Monad }

trait KleisliSyntax {
  implicit def kleisliSyntax[F[_], E, A](action: DisjunctionT[F, E, A]): KleisliOps[F, E, A] =
    new KleisliOps(action)
}

class KleisliOps[F[_], E, A](action: DisjunctionT[F, E, A]) {
  def retrying(handler: E => DisjunctionT[F, E, A])(implicit F: Monad[F]): Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A] =
    Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A](_.retrying(action)(handler))

  def recovering(handler: E => Count)(implicit F: Monad[F]): Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A] =
    Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A](_.recovering(action)(handler))

  def recoverAll(implicit F: Monad[F]): Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A] =
    Kleisli[DisjunctionT[F, E, ?], RetryPolicy, A](_.recoverAll(action))
}
