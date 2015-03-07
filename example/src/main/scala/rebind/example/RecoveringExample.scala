package rebind.example

import rebind._
import rebind.Rebind._

import scala.concurrent.duration._

import scalaz.DisjunctionT
import scalaz.effect.{ IO, SafeApp }

/* Expected output:
Running action: Uh
Running action: Uh // 1 second delay
Running action: Oh // 2 second delay
Running action: Oh // 4 second delay
Running action: Oh // 8 second delay
Ended with error
*/
object RecoveringExample extends SafeApp {
  sealed abstract class UhOh
  final case object Uh extends UhOh
  final case object Oh extends UhOh

  // Emulate an action that fails the first 2 times with A, and subsequent times with B
  private var count = 0
  def ew: DisjunctionT[IO, UhOh, Unit] =
    DisjunctionT.right(IO.putStr("Running action: ")).flatMap { _ =>
      if (count < 2) DisjunctionT.left(IO { count += 1; println("Uh"); Uh })
      else DisjunctionT.left(IO { println("Oh"); Oh })
    }

  override def runc: IO[Unit] = {
    // Infinite retry, exponential backoff starting with 1 second
    val policy = exponentialBackoff(1.second)

    val retryingAction =
      policy.boundError(ew) {
        case Uh => 5.times // Retry on Uh as long as total # of retries is < 5
        case Oh => 4.times // Retry on Oh as long as total # of retries is < 4
      }

    retryingAction.run.flatMap {
      _.fold(_ => IO.putStrLn("Ended with error"),
             _ => IO.putStrLn("Ended with success"))
    }
  }
}
