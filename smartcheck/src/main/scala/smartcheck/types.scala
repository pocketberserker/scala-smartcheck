package smartcheck

import scalaz._
import scalaz.std.anyVal._

final case class Idx(level: Int, column: Int)

object Idx {

  implicit val idxEqual = new Equal[Idx] {
    override def equal(a1: Idx, a2: Idx) = a1 == a2
  }

  implicit val idxShow = Show.show((i: Idx) =>
    Cord("Idx(", intInstance.show(i.level), ", ", intInstance.show(i.column), ")")
  )
}

sealed abstract class Subst
case object Keep extends Subst
case object SubstC extends Subst

object Subst {

  implicit val substEqual = new Equal[Subst] {
    override def equal(fa1: Subst, fa2: Subst) = (fa1, fa2) match {
      case (Keep, Keep) => true
      case (SubstC, SubstC) => true
      case _ => false
    }
  }

  implicit val substShow = Show.shows[Subst](_ match {
    case Keep => "Keep"
    case SubstC => "Subst"
  })
}

final case class Replace[A](unVals: List[A], unConstrs: List[A])
