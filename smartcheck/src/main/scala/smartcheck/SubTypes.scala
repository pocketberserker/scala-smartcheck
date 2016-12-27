package smartcheck

import scalaz.Show
import scalaz.TreeLoc.TreeForest
import scalaprops._
import shapeless._

trait SubT {
  type A
  def G: Gen[A]
  def S: SubTypes[A]
  def unSubT: A
}

abstract class SubTypes[A: Gen] extends Show[A] {
  def subTypes(a: A): TreeForest[SubT]
  def baseType(a: A): Boolean = false
  def replaceChild[B](a: A, forest: TreeForest[Subst], b: B): Option[A]
  def toConstr(a: A): String
  def showForest(a: A): TreeForest[String]
}

class DefaultSubTypes[A: Gen] {

  def apply[R]()(implicit G1: Generic.Aux[A, R], G2: GST[R]): SubTypes[A] =
    new SubTypes[A] {
      override def subTypes(a: A) = G2.gst(G1.to(a))
      override def replaceChild[B](a: A, forest: TreeForest[Subst], b: B) =
        G2.grc(G1.to(a), forest, b).map(G1.from(_))
      override def toConstr(a: A) = G2.gtc(G1.to(a))
      override def showForest(a: A) = G2.gsf(G1.to(a))
    }

  def baseType[R](f: A => Boolean)(implicit G1: Generic.Aux[A, R], G2: GST[R]): SubTypes[A] =
    new SubTypes[A] {
      override def subTypes(a: A) = G2.gst(G1.to(a))
      override def replaceChild[B](a: A, forest: TreeForest[Subst], b: B) =
        G2.grc(G1.to(a), forest, b).map(G1.from(_))
      override def toConstr(a: A) = G2.gtc(G1.to(a))
      override def showForest(a: A) = G2.gsf(G1.to(a))
      override def baseType(a: A) = f(a)
    }
}

object SubTypes extends SubTypesInstances {

  def default[A: Gen] = new DefaultSubTypes[A]
}

abstract class SubTypesInstances {

  //implicit def boolSubTypes(implicit G0: Gen[Boolean]): SubTypes[Boolean] =
  //  new DefaultSubTypes[Boolean].baseType(_ => true)
}
