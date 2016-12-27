package smartcheck
package examples

import scalaprops._
import scalaprops.AsProperty._
import shapeless._
import GSTAuto._

object ColorInstances {

  implicit val genColor = Gen.elements(Color.r, Color.b, Color.bb, Color.nb)

  implicit val subTypesColor: SubTypes[Color] =
    SubTypes.default.baseType(_ => true)
}

object Main extends App {

  import Color._
  import ColorInstances._
  import RBSet._

  def blackDepth[A](s: RBSet[A]): Option[Int] = s match {
    case E() => Some(1)
    case T(R, l, _, r) => (blackDepth(l), blackDepth(r)) match {
      case (Some(n), Some(m)) if n == m => Some(n)
      case (_, _) => None
    }
    case T(B, l, _, r) => (blackDepth(l), blackDepth(r)) match {
      case (Some(n), Some(m)) if n == m => Some(1 + n)
      case (_, _) => None
    }
    case _ => ???
  }

  implicit def genRBSet[A](implicit G: Gen[A]) = {

    def tree(c: Color): Int => Gen[RBSet[A]] = (n: Int) =>
      (c, n) match {
        case (B, 0) => Gen.value(E())
        case (B, n) =>
          Gen.oneOf(
            Gen.value(E()),
            for {
              l <- tree(B)(n / 2)
              x <- G
              r <- tree(B)(n / 2)
            } yield T(B, l, x, r),
            for {
              l <- tree(R)(n / 2)
              x <- G
              r <- tree(R)(n / 2)
            } yield T(R, l, x, r)
          )
        case (R, 0) => Gen.value(E())
        case (R, n) =>
          Gen.oneOf(
            Gen.value(E()),
            for {
              l <- tree(B)(n / 2)
              x <- G
              r <- tree(B)(n / 2)
            } yield T(B, l, x, r)
          )
      }

    Gen.sized(tree(R))
  }

  val `black balanced` = (t: RBSet[Int]) => blackDepth(t).isDefined

  implicit def subTypesRBSet[A: SubTypes : Gen]
    (implicit G: GST[E[A] :+: EE[A] :+: T[A] :+: CNil])
    : SubTypes[RBSet[A]] =
    SubTypes.default[RBSet[A]].apply()

  // TODO: GST[E[A] :+: EE[A] :+: T[A] :+: CNil] instance
  //SmartCheck.run(ScArgs())(`black balanced`)
}
