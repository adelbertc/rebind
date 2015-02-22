# Rebind (WIP)
This is still a work in progress!

## Overview
Rebind is a Scala port/remake of the Haskell [retry](https://hackage.haskell.org/package/retry) library. One
of the main differences is it is designed to work with `DisjunctionT`'s instead of `MonadIO` things.

### Example Usage
```scala
import rebind._
import rebind.Rebind._

import scala.concurrent.duration._

import scalaz.DisjunctionT
import scalaz.effect.{ IO, SafeApp }

object Example extends SafeApp {
  sealed abstract class UhOh
  final case object Oops extends UhOh

  override def runc: IO[Unit] = {
    val policy = exponentialBackoff(1.second) && limitRetries(3)

    val action: DisjunctionT[IO, UhOh, Unit] =
      DisjunctionT.right(IO.putStrLn("Running action")).flatMap(_ => DisjunctionT.left(IO(Oops)))

    policy.retrying(action) {
      case Oops => action // retry same action
    }.run.flatMap(_.fold(_ => IO.putStrLn("Ended with error"), _ => IO.putStrLn("Ended with success")))
  }
}

/*
Running action    // immediate
Running action    // 1 second pause
Running action    // 2 second pause
Running action    // 4 second pause
Ended with error  // immediate
*/
```

## License
Code provided under the BSD-3 license available at http://opensource.org/licenses/BSD-3-Clause, as
well as in the LICENSE file. This is the same license used as the retry library.

## TODO
* `recovering` is a bit weird, it does not take into account that errors can change from iteration to iteration
* Reader for passing in RetryPolicy
