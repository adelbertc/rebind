package rebind

import org.scalacheck._
import org.scalacheck.Arbitrary._

import scalaz.Equal

final case object Oops {
  implicit val oopsInstance: Equal[Oops.type] = Equal.equalA
}

sealed abstract class UhOh
final case object Uh extends UhOh
final case object Oh extends UhOh

object UhOh {
  implicit val uhOhInstance: Equal[UhOh] = Equal.equalA

  implicit val uhOhArbitrary: Arbitrary[UhOh] =
    Arbitrary(Gen.oneOf(List[UhOh](Uh, Oh)))
}
