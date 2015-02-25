package rebind
package syntax

trait CountSyntax {
  implicit def countSyntax(i: Int): CountOps = new CountOps(i)
}

class CountOps(val i: Int) extends AnyVal {
  def times: Count = Count.Finite(i)

  def time: Count = times
}
