package smartcheck

import scalaprops._
import scalaz.TreeLoc.TreeForest
import DataToTree._
import SmartGen._

object Extrapolate {

  import Render._

  def extrapolate[A : SubTypes](args: ScArgs, d: A, origProp: A => Property): List[Idx] = {
    def test(a: A, idx: Idx): (Int, Result[A]) =
      iterateArbIdx(d, idx, args.scMaxDepth, args.scMaxForall, args.scMaxSize, origProp)
    def next(a: A, t: (Int, Result[A]), forest: TreeForest[Boolean], idx: Idx, idxs: List[Idx]): (A, List[Idx]) =
      t._2 match {
        case Result.FailedProp() if args.scMinForall < t._1 =>
          iter(d, test, next, origProp, args.scMaxDepth, forestReplaceChildren(forest, idx, false), idx.copy(column = idx.column + 1), idx :: idxs)
        case _ =>
          iter(d, test, next, origProp, args.scMaxDepth, forest, idx.copy(column = idx.column + 1), idxs)
      }
    println()
    smartPrintLn("Extrapolating values ...")
    val forest = mkSubstForest(d, true)
    val t = iter(d, test, next, origProp, args.scMaxDepth, forest, Idx(0, 0), List())
    t._2
  }
}
