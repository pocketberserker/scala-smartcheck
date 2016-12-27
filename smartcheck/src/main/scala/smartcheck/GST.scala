package smartcheck

import scalaz.TreeLoc.TreeForest
import shapeless._

trait GST[A] {
  def gst(a: A): TreeForest[SubT]
  def grc[B](a: A, forest: TreeForest[Subst], b: B): Option[A]
  def gtc(a: A): String
  def gsf(a: A): TreeForest[String]
  def gsz(a: A): Int
}

object GSTAuto extends TypeClassCompanion[GST] {
  override val typeClass: TypeClass[GST] = new GSTTypeClassImpl()
}

private final class GSTTypeClassImpl() extends TypeClass[GST] {

  override def coproduct[L, R <: Coproduct](cl: => GST[L], cr: => GST[R]) = new GST[L :+: R] {
    override def gst(a: L :+: R) = a match {
      case Inl(l) => cl.gst(l)
      case Inr(r) => cr.gst(r)
    }
    override def grc[B](a: L :+: R, forest: TreeForest[Subst], b: B) = a match {
      case Inl(l) => cl.grc(l, forest, b).map(Inl(_))
      case Inr(r) => cr.grc(r, forest, b).map(Inr(_))
    }
    override def gtc(a: L :+: R) = a match {
      case Inl(l) => cl.gtc(l)
      case Inr(r) => cr.gtc(r)
    }
    override def gsf(a: L :+: R) = a match {
      case Inl(l) => cl.gsf(l)
      case Inr(r) => cr.gsf(r)
    }
    override def gsz(a: L :+: R) = a match {
      case Inl(l) => cl.gsz(l)
      case Inr(r) => cr.gsz(r)
    }
  }

  override def product[H, T <: HList](ch: GST[H], ct: GST[T]) = new GST[H :: T] {
    override def gst(a: H :: T) = ch.gst(a.head) ++ ct.gst(a.tail)
    override def grc[B](a: H :: T, forest: TreeForest[Subst], b: B) =
      if (forest.isEmpty) {
        Some(a)
      }
      else {
        val s = forest.splitAt(ch.gsz(a.head))
        for {
          left <- ch.grc(a.head, s._1, b)
          right <- ct.grc(a.tail, s._2, b)
        } yield (left :: right)
      }
    override def gtc(a: H :: T) = ch.gtc(a.head) + ct.gtc(a.tail)
    override def gsf(a: H :: T) = ch.gsf(a.head) ++ ct.gsf(a.tail)
    override def gsz(a: H :: T) = ch.gsz(a.head) + ct.gsz(a.tail)
  }

  override def project[F, G](instance: => GST[G], to: F => G, from: G => F) = new GST[F] {
    override def gst(a: F) = instance.gst(to(a))
    override def grc[B](a: F, forest: TreeForest[Subst], b: B) =
      instance.grc(to(a), forest, b).map(from)
    override def gtc(a: F) = instance.gtc(to(a))
    override def gsf(a: F) = instance.gsf(to(a))
    override def gsz(a: F) = instance.gsz(to(a))
  }

  override val emptyCoproduct = new GST[CNil] {
    override def gst(a: CNil) = Stream()
    override def grc[B](a: CNil, forest: TreeForest[Subst], b: B) = None
    override def gtc(a: CNil) = ""
    override def gsf(a: CNil) = Stream()
    override def gsz(a: CNil) = 0
  }

  override val emptyProduct = new GST[HNil] {
    override def gst(a: HNil) = Stream()
    override def grc[B](a: HNil, forest: TreeForest[Subst], b: B) = None
    override def gtc(a: HNil) = ""
    override def gsf(a: HNil) = Stream()
    override def gsz(a: HNil) = 0
  }
}
