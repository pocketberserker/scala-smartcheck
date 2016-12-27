package smartcheck
package examples

import scalaz.{Order, Equal}
import scalaz.syntax.order._
import scalaz.std.list._
import Color._

sealed abstract class RBSet[A] {
  import RBSet._

  def redden: RBSet[A] = this match {
    case E() => throw new Exception("cannot redden empty tree")
    case EE() => throw new Exception("cannot redden empty tree")
    case T(_, a, x, b) => T(R, a, x, b)
  }

  def blacken: RBSet[A] = this match {
    case E() => E()
    case EE() => E()
    case T(_, a, x, b) => T(B, a, x, b)
  }

  def isBB: Boolean = this match {
    case EE() => true
    case T(BB, _, _, _) => true
    case _ => false
  }

  def blacker_ : RBSet[A] = this match {
    case E() => EE()
    case T(c, l, x, r) => T(c.blacker, l, x, r)
    case _ => ???
  }

  def redder_ : RBSet[A] = this match {
    case EE() => E()
    case T(c, l, x, r) => T(c.redder, l, x, r)
    case _ => ???
  }

  def member(x: A)(implicit O: Order[A]): Boolean = this match {
    case E() => false
    case EE() => ???
    case T(_, l, y, r) =>
      if (x < y) l.member(x)
      else if (x > y) r.member(x)
      else true
  }

  def max: A = this match {
    case E() => throw new Exception("no largest element")
    case EE() => ???
    case T(_, _, x, E()) => x
    case T(_, _, x, r) => r.max
  }

  def insert(x: A)(implicit O: Order[A]): RBSet[A] = {
    def ins(s: RBSet[A]): RBSet[A] = s match {
      case E() => T(R, E(), x, E())
      case EE() => ???
      case s@T(color, a, y, b) =>
        if (x < y) balance(color, ins(a), y, b)
        else if (x > y) balance(color, a, y, ins(b))
        else s
    }
    ins(this).blacken
  }

  def delete(x: A)(implicit O: Order[A]): RBSet[A] = {
    def del(s: RBSet[A]): RBSet[A] = s match {
      case E() => E()
      case EE() => ???
      case s@T(color, a, y, b) =>
        if (x < y) bubble(color, del(a), y, b)
        else if (x > y) bubble(color, a, y, del(b))
        else s.remove
    }
    del(this).blacken
  }

  def remove: RBSet[A] = this match {
    case E() => E()
    case EE() => ???
    case T(R, E(), _, E()) => E()
    case T(B, E(), _, E()) => EE()
    case T(B, E(), _, T(R, a, x, b)) => T(B, a, x, b)
    case T(B, T(R, a, x, b), _, E()) => T(B, a, x, b)
    case T(color, l, _, r) => bubble(color, l.removeMax, l.max, r)
  }

  def removeMax: RBSet[A] = this match {
    case E() => throw new Exception("no maximum to remove")
    case EE() => ???
    case s@T(_, _, _, E()) => s.remove
    case s@T(color, l, x, r) => bubble(color, l, x, r.removeMax)
  }

  def toAscList: List[A] = this match {
    case E() => List()
    case EE() => ???
    case T(_, l, x, r) => l.toAscList ++ List(x) ++ r.toAscList
  }
}

object RBSet extends RBSetInstances {

  final case class E[A]() extends RBSet[A]
  final case class EE[A]() extends RBSet[A]
  final case class T[A](color: Color, left: RBSet[A], value: A, right: RBSet[A]) extends RBSet[A]

  def empty[A]: RBSet[A] = E[A]()

  def balance[A](color: Color, aa: RBSet[A], xx: A, bb: RBSet[A]): RBSet[A] = (color, aa, xx, bb) match {
    case (B, T(R, T(R, a, x, b), y, c), z, d) => T(R, T(B, a, x, b), y, T(B, c, z, d))
    case (B, T(R, a, x, T(R, b, y, c)), z, d) => T(R, T(B, a, x, b), y, T(B, c, z, d))
    case (B, a, x, T(R, T(R, b, y, c), z, d)) => T(R, T(B, a, x, b), y, T(B, c, z, d))
    case (B, a, x, T(R, b, y, T(R, c, z, d))) => T(R, T(B, a, x, b), y, T(B, c, z, d))
    case (BB, T(R, T(R, a, x, b), y, c), z, d) => T(B, T(B, a, x, b), y, T(B, c, z, d))
    case (BB, T(R, a, x, T(R, b, y, c)), z, d) => T(B, T(B, a, x, b), y, T(B, c, z, d))
    case (BB, a, x, T(R, T(R, b, y, c), z, d)) => T(B, T(B, a, x, b), y, T(B, c, z, d))
    case (BB, a, x, T(R, b, y, T(R, c, z, d))) => T(B, T(B, a, x, b), y, T(B, c, z, d))
    case (BB, a, x, T(NB, T(B, b, y, c), z, d@(T(B, _, _, _)))) =>
      T(B, T(B, a, x, b), y, balance(B, c, z, d.redden))
    case (BB, T(NB, a@(T(B, _, _, _)), x, T(B, b, y, c)), z, d) =>
      T(B, balance(B, a.redden, x, b), y, T(B, c, z, d))
    case (color, a, x, b) => T(color, a, x, b)
  }

  def bubble[A](color: Color, l: RBSet[A], x: A, r: RBSet[A]): RBSet[A] =
    if (l.isBB || r.isBB) balance(color.blacker, l.redder_, x, r.redder_)
    else balance(color, l, x, r)
}

abstract class RBSetInstances {
  implicit def equalInstance[A: Equal]: Equal[RBSet[A]] = new Equal[RBSet[A]] {
    override def equal(a1: RBSet[A], a2: RBSet[A]) =
      Equal[List[A]].equal(a1.toAscList, a2.toAscList)
  }
}
