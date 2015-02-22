package rebind
package syntax

import scalaz.{ DisjunctionT, Monad }

trait ReaderSyntax {
  implicit def readerSyntax[F[_], E, A](action: DisjunctionT[F, E, A]): ReaderOps[F, E, A] =
    new ReaderOps(action)
}

class ReaderOps[F[_], E, A](action: DisjunctionT[F, E, A]) {
  def retrying(handler: E => DisjunctionT[F, E, A])(implicit F: Monad[F]): RetryPolicy => DisjunctionT[F, E, A] =
    _.retrying(action)(handler)

  def recovering(handler: E => Count)(implicit F: Monad[F]): RetryPolicy => DisjunctionT[F, E, A] =
    _.recovering(action)(handler)

  def recoverAll(implicit F: Monad[F]): RetryPolicy => DisjunctionT[F, E, A] = _.recoverAll(action)
}
