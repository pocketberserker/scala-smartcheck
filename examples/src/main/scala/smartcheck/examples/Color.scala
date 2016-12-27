package smartcheck
package examples

sealed abstract class Color {
  import Color._

  def blacker: Color = this match {
    case NB => R
    case R => B
    case B => BB
    case BB => throw new Exception("too black")
  }

  def redder: Color = this match {
    case NB => throw new Exception("not black enough")
    case R => NB
    case B => R
    case BB => B
  }
}

object Color {
  case object R extends Color
  case object B extends Color
  case object BB extends Color
  case object NB extends Color

  val r: Color = R
  val b: Color = B
  val bb: Color = BB
  val nb: Color = NB
}
