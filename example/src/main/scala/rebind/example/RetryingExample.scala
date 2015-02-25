package rebind.example

import rebind._
import rebind.Rebind._

import scala.concurrent.duration._

import scalaz.DisjunctionT
import scalaz.effect.{ IO, SafeApp }

/* Expected output:
Running action
Running action    // 1 second delay
Running action    // 2 second delay
Running action    // 4 second delay
Ended with error
*/
object RetryingExample extends SafeApp {
  final case object Oops

  override def runc: IO[Unit] = {
    // Exponential backoff starting with 1 second, up to 3 times
    val policy = exponentialBackoff(1.second) && limitRetries(3)

    // Action that always fails with Oops
    val action: DisjunctionT[IO, Oops.type, Unit] =
      DisjunctionT.right(IO.putStrLn("Running action")).flatMap(_ => DisjunctionT.left(IO(Oops)))

    // Retry same action regardless of error
    val retryingAction = policy.retrying(action)(_ => action)

    retryingAction.run.flatMap {
      _.fold(_ => IO.putStrLn("Ended with error"),
             _ => IO.putStrLn("Ended with success"))
    }
  }
}
