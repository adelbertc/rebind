package rebind

import scalaz.{Disjunction, DisjunctionT, Name}

final class TestAction[E, A](private var errorStream: Stream[E], private val success: A) {
  def this(n: Int, error: E, success: A) = this(Stream.fill(n)(error), success)

  def run(): DisjunctionT[Name, E, A] =
    DisjunctionT {
      Name {
        errorStream match {
          case Stream.Empty => Disjunction.right(success)
          case e #:: es =>
            errorStream = es
            Disjunction.left(e)
        }
      }
    }
}
