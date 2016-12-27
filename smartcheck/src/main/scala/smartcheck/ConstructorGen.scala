package smartcheck

import scalaz._
import scalaz.TreeLoc.TreeForest
import scalaz.std.string._
import scalaprops._
import shapeless._

object ConstructorGen {

  import DataToTree._
  import SmartGen._
  import Render._

  def constructorGen[A : SubTypes, C <: shapeless.Coproduct, K <: HList]
    (args: ScArgs, d: A, prop: A => Property, vs: List[Idx])
    (implicit gen: LabelledGeneric.Aux[A, C],
      keys: ops.union.Keys.Aux[C, K],
      toList: ops.hlist.ToTraversable.Aux[K, List, Symbol]): List[Idx] = {

    def test(x: A, idx: Idx): Boolean = {
      val res = extrapolateConstrs(args, x, idx, prop)
      !vs.exists(_ == idx) && res
    }

    def next(a: A, res: Boolean, forest: TreeForest[Boolean], idx: Idx, idxs: List[Idx]): (A, List[Idx]) =
      iter(
        d,
        test,
        next,
        prop,
        args.scMaxDepth,
        if(res) forestReplaceChildren(forest, idx, false) else forest,
        idx.copy(column = idx.column + 1),
        if(res) idx :: idxs else idxs
      )

    println("")
    smartPrintLn("Extrapolating Constructors ...")
    val forest =
      vs.foldLeft(mkSubstForest(d, true)){ case (f, idx) =>
        forestReplaceChildren(f, idx, false)
      }
    iter(d, test, next, prop, args.scMaxDepth, forest, Idx(0, 0), List())
      ._2
  }

  private[this] class Names[A] {
    def apply[C <: shapeless.Coproduct, K <: HList]()
      (implicit gen: LabelledGeneric.Aux[A, C],
        keys: ops.union.Keys.Aux[C, K],
        toList: ops.hlist.ToTraversable.Aux[K, List, Symbol]): ISet[String] =
      ISet.fromList(toList(keys()).map(_.name))
  }

  private[this] def names[A] = new Names[A]

  private[this] def extrapolateConstrs[A : SubTypes, C <: shapeless.Coproduct, K <: HList]
    (args: ScArgs, a: A, idx: Idx, prop: A => Property)
    (implicit gen: LabelledGeneric.Aux[A, C],
      keys: ops.union.Keys.Aux[C, K],
      toList: ops.hlist.ToTraversable.Aux[K, List, Symbol]): Boolean = {

    @annotation.tailrec
    def recConstrs(constrs: ISet[String]): Boolean = {
      val allConstrs = names[A]()
      if(allConstrs.isSubsetOf(constrs)) true
      // TODO: prop compose expectFailure
      else arbSubset(args, a, idx, prop, constrs) match {
        case Result.Satisfied(x) =>
          recConstrs(constrs.insert(subConstr(x, idx, args.scMaxDepth)))
        case _ => false
      }
    }

    recConstrs(ISet.singleton(subConstr(a, idx, args.scMaxDepth)))
  }

  private[this] def arbSubset[A : SubTypes](args: ScArgs, a: A, idx: Idx, prop: A => Property, constrs: ISet[String]): Result[A] = {
    def p(b: A): Property =
      Property.implies(
        !constrs.member(subConstr(b, idx, args.scMaxDepth)),
        prop(b))
    iterateArbIdx(a, idx, args.scMaxDepth, args.scMaxExists, args.scMaxSize, p)
      ._2
  }

  private[this] def subConstr[A : SubTypes](x: A, idx: Idx, max: Option[Int]): String =
    getAtIdx(x, idx, max) match {
      // TODO: fix exception type
      case None => throw new Exception("constrs'")
      case Some(xx) => xx.S.toConstr(xx.unSubT)
    }
}
