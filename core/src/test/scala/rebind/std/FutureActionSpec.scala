package rebind
package std

import org.specs2.Specification

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

import scalaz.{Disjunction, DisjunctionT}

class FutureActionSpec extends Specification {
  def is =
    s2"""
    Success   ${futureSuccess}
    Failure   ${futureFailure}
    Retry     ${futureRetry}
    """

  def testFuture[A](have: DisjunctionT[FutureAction, Throwable, A], expected: Disjunction[Throwable, A]) =
    Await.result(have.run.unsafeRun(), Duration.Inf) mustEqual expected

  def futureSuccess = {
    val r = "future"
    val future = FutureAction(Future.successful(r))
    val retried = RetryPolicy.immediate.retryAll(future)
    testFuture(retried, Disjunction.right(r))
  }

  def futureFailure = {
    val r = new Exception("oops")
    val future = FutureAction[String](Future.failed(r))
    val retried = RetryPolicy.limitRetries(5).retryAll(future)
    testFuture(retried, Disjunction.left(r))
  }


  final case object FutureException extends Exception
  def futureRetry = {
    val failingFuture = FutureAction[String](Future.failed(FutureException))
    val r = "future"
    val successfulFuture = FutureAction(Future.successful(r))
    val retried = RetryPolicy.immediate.recoverWith(failingFuture) { case FutureException => successfulFuture }
    testFuture(retried, Disjunction.right(r))
  }
}
