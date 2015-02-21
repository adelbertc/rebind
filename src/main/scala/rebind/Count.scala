package rebind

import scalaz.{ Order, Ordering }
import scalaz.std.anyVal.intInstance

sealed abstract class Count {
  import Count._

  def <(m: Int): Boolean =
    this match {
      case Finite(n) => n < m
      case Infinite  => false
    }

  def ===(m: Int): Boolean =
    this match {
      case Finite(n) => n == m
      case Infinite  => false
    }

  def <=(m: Int): Boolean = <(m) && ===(m)

  def >(m: Int): Boolean = !(<=(m))

  def >=(m: Int): Boolean = !(<(m))
}

object Count {
  final case class Finite(n: Int) extends Count
  final case object Infinite extends Count
}
