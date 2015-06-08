package rebind.std

import scala.concurrent.{ExecutionContext, Future}

import scalaz.{Disjunction, DisjunctionT, Kleisli, Monad}
import scalaz.std.scalaFuture._

/** Suspended Future that "always succeeds" - the contained value indicates success/failure via scalaz.Disjunction. */
final class FutureAction[A] private[std] (private val suspended: Kleisli[Future, Unit, A])(implicit ec: ExecutionContext) {
  def flatMap[B](f: A => FutureAction[B]): FutureAction[B] =
    new FutureAction(suspended.flatMap(a => f(a).suspended))

  def map[B](f: A => B): FutureAction[B] =
    new FutureAction(suspended.map(f))

  def unsafeRun(): Future[A] = suspended.run(())
}

object FutureAction { companionObject =>
  /** Convert a standard library Future into a DisjunctionT[FutureAction, Throwable, A].
    *
    * This allows you to use Futures with this library.
    *
    * Note a DisjunctionT[FutureAction, Throwable, A] is similar to a
    * Future[Disjunction[Throwable, A]]. A FutureAction can be seen as a suspended Future
    * that can be retried. Moreover, while the standard library Future
    * has error-handling "built in", the transformed "Future" returned by this function
    * will always "succeed." The value contained within the Future will be either
    * a Disjunction left with a Throwable signaling an error (what would normally
    * be seen as a Failure), or a Disjunction right with the value we want (what
    * would normally be seen as a Success).
    */
  def apply[A](future: => Future[A])(implicit ec: ExecutionContext): DisjunctionT[FutureAction, Throwable, A] = {
    val suspended =
      Kleisli.kleisli[Future, Unit, Disjunction[Throwable, A]] { _ =>
        future.map(Disjunction.right[Throwable, A]).recover { case t => Disjunction.left(t) }
      }

    DisjunctionT(new FutureAction(suspended))
  }

  def point[A](a: => A)(implicit ec: ExecutionContext): FutureAction[A] =
    new FutureAction(Monad[Kleisli[Future, Unit, ?]].point(a))

  implicit def futureActionMonad(implicit ec: ExecutionContext): Monad[FutureAction] =
    new Monad[FutureAction] {
      def bind[A, B](fa: FutureAction[A])(f: A => FutureAction[B]): FutureAction[B] =
        fa.flatMap(f)

      def point[A](a: => A): FutureAction[A] = companionObject.point(a)
    }
}
