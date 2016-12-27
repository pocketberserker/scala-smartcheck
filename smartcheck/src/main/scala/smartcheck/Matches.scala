package smartcheck

import scalaz._

object Matches {

  import DataToTree._
  import SmartGen._

  def matchesShapes[A : SubTypes](d: A, ls: List[(A, Replace[Idx])]): Boolean =
    ls.exists(l => matchesShape(d, l))

  private def matchesShape[A](a: A, t: (A, Replace[Idx]))(implicit S: SubTypes[A]): Boolean = {
    def bSub(idx: Idx): Option[SubT] = getAtIdx(t._1, idx, None)
    def updateA(idx: Idx, d: A): Option[A] = bSub(idx).map(x => replace(d, idx, x)).getOrElse(None)
    def go(ma: Option[A], idx: Idx): Option[A] =
      ma.map(x => updateA(idx, x)).getOrElse(None)
    def foldEqConstrs(tt: (Tree[SubT], Tree[SubT])): Boolean = {
      val s0 = tt._1.rootLabel
      val l0 = s0.unSubT
      val sts0 = tt._1.subForest
      val s1 = tt._2.rootLabel
      val l1 = s1.unSubT
      val sts1 = tt._2.subForest
      lazy val next = sts0.zip(sts1).forall(foldEqConstrs)
      if(s0.S.baseType(l0) && s1.S.baseType(l1)) next
      else if(s0.S.toConstr(l0) == s1.S.toConstr(l1)) next
      else false
    }
    if(S.baseType(a) && S.baseType(t._1)) true
    else if(S.toConstr(a) != S.toConstr(t._1)) false
    else {
      (t._2.unVals ++ t._2.unConstrs).foldLeft(Some(a): Option[A])(go) match {
        case Some(aa) => {
          val x = S.subTypes(aa)
          val y = S.subTypes(t._1)
          x.zip(y).forall(foldEqConstrs)
        }
        case None => false
      }
    }
  }
}
