package rebind.syntax.std

import rebind.std.FutureAction

import scala.concurrent.{ExecutionContext, Future}

import scalaz.DisjunctionT

trait FutureSyntax {
  implicit def futureSyntax[A](future: => Future[A])(implicit ec: ExecutionContext): FutureOps[A] =
    new FutureOps(future)
}

class FutureOps[A](future: => Future[A])(implicit ec: ExecutionContext) {
  def action: DisjunctionT[FutureAction, Throwable, A] = FutureAction(future)
}
