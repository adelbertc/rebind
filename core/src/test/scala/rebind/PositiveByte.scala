package rebind

import org.scalacheck._
import org.scalacheck.Arbitrary._

final case class PositiveByte(byte: Byte) extends AnyVal {
  def int: Int = byte.toInt
}

object PositiveByte {
  implicit val positiveByteArbitrary: Arbitrary[PositiveByte] =
    Arbitrary(Gen.chooseNum[Byte](1, Byte.MaxValue).map(PositiveByte.apply))
}
