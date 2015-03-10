package rebind

import org.scalacheck._
import org.scalacheck.Arbitrary._

final class PositiveByte private(val byte: Byte) extends AnyVal {
  def int: Int = byte.toInt
}

object PositiveByte {
  implicit val positiveByteArbitrary: Arbitrary[PositiveByte] =
    Arbitrary(Gen.chooseNum[Byte](1, Byte.MaxValue).map(b => new PositiveByte(b)))
}

final class PositiveInt private(val int: Int) extends AnyVal

object PositiveInt {
  implicit val positiveIntArbitrary: Arbitrary[PositiveInt] =
    Arbitrary(Gen.chooseNum[Int](1, Int.MaxValue).map(i => new PositiveInt(i)))
}
