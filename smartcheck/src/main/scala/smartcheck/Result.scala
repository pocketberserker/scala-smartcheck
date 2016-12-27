package smartcheck

import scalaz._

sealed abstract class Result[A] {

  import Result._

  def map[B](f: A => B): Result[B] = this match {
    case BaseType() => baseType
    case FailedPreCond() => failedPreCond
    case FailedProp() => failedProp
    case Satisfied(a) => Satisfied(f(a))
  }

  def flatMap[B](f: A => Result[B]): Result[B] = this match {
    case BaseType() => baseType
    case FailedPreCond() => failedPreCond
    case FailedProp() => failedProp
    case Satisfied(a) => f(a)
  }
}

object Result {

  final case class BaseType[A]() extends Result[A]
  final case class FailedPreCond[A]() extends Result[A]
  final case class FailedProp[A]() extends Result[A]
  final case class Satisfied[A](a: A) extends Result[A]

  final def baseType[A]: Result[A] = BaseType()
  final def failedPreCond[A]: Result[A] = FailedPreCond()
  final def failedProp[A]: Result[A] = FailedProp()
  final def satisfied[A](a: A): Result[A] = Satisfied(a)

  implicit val resultMonad: Monad[Result] = new Monad[Result] {
    override final def map[A, B](fa: Result[A])(f: A => B) = fa.map(f)
    override final def bind[A, B](fa: Result[A])(f: A => Result[B]) = fa.flatMap(f)
    override final def point[A](a: => A) = satisfied(a)
  }

  implicit def resultEqual[A](implicit A: Equal[A]): Equal[Result[A]] = new Equal[Result[A]] {
    override final def equal(fa1: Result[A], fa2: Result[A]) = (fa1, fa2) match {
      case (BaseType(), BaseType()) => true
      case (FailedPreCond(), FailedPreCond()) => true
      case (FailedProp(), FailedProp()) => true
      case (Satisfied(a1), Satisfied(a2)) => A.equal(a1, a2)
      case _ => false
    }
  }

  implicit def resultShow[A](implicit A: Show[A]): Show[Result[A]] =
    Show.show(_ match {
      case BaseType() => "BaseType"
      case FailedPreCond() => "FailedPreCond"
      case FailedProp() => "FailedProp"
      case Satisfied(a) => Cord("Satisfied(", A.show(a), ")")
    })
}

