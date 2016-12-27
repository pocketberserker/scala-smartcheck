package smartcheck

import scalaprops._
import scalaz._
import scalaz.TreeLoc.TreeForest

object SmartGen {

  import DataToTree._

  def iterateArbIdx[A : SubTypes](d: A, idx: Idx, max: Option[Int], tries: Int, sz: Int, prop: A => Property): (Int, Result[A]) =
    getAtIdx(d, idx, max) match {
      // TODO: fix exception type
      case None => throw new Exception("iterateArb 0")
      case Some(ext) => if(ext.S.baseType(ext.unSubT)) (0, Result.baseType[A]) else iterateArb(d, ext, idx, tries, sz, prop)
    }

  def iterateArb[A](d: A, ext: SubT, idx: Idx, tries: Int, max: Int, prop: A => Property)(implicit S: SubTypes[A]): (Int, Result[A]) = {
    def newMax(s: SubT): Int = valDepth(s.unSubT)(s.S)
    @annotation.tailrec
    def go(i: Int, res: Result[A], g: Rand, tr: Int, currMax: Int): (Int, Result[A]) =
      if(tr >= tries) (i, res)
      else {
        val (g0, size) = g.choose(0, currMax)
        val s = newVal(ext, g, size)
        if(newMax(s) <= max) go(i, res, g0, tr + 1, 0)
        else {
          replace(d, idx, s) match {
            // TODO: fix exception type
            case None => throw new Exception("iterateArb 1")
            case Some(dd) => resultify(prop, dd) match {
              case Result.FailedPreCond() => go(i, Result.failedPreCond[A], g0, tr + 1, (currMax + 1) * 2)
              case Result.FailedProp() => go(i + 1, Result.failedProp[A], g0, tr + 1, (currMax + 1) * 2)
              case Result.Satisfied(x) => (i + 1, Result.satisfied(x))
              // TODO: fix exception type
              case Result.BaseType() => throw new Exception("baseType form resultify")
            }
          }
        }
      }
    val r = Rand.fromSeed()
    go(0, Result.failedPreCond[A], r, 0, 0)
  }

  private[this] def valDepth[A: SubTypes](d: A): Int = depth(mkSubstForest(d, true))

  private[this] def newVal(s: SubT, g: Rand, size: Int): SubT = {
    val m = s.G.resize(size)
    new SubT {
      type A = s.A
      def G = m
      def S = s.S
      def unSubT = m.f(size, g)._2
    }
  }

  def replace[A : SubTypes](d: A, idx: Idx, sub: SubT): Option[A] =
    replaceAtIdx(d, idx, sub.unSubT)

  def resultify[A](prop: A => Property, a: A): Result[A] = {
    // TODO: fix exception type
    def err[B]():B = throw new Exception("resultify: should not evaluate.")
    val f = prop(a).f
    // FIXME
    val r = \/.fromTryCatchThrowable[(Rand, scalaprops.Result), Throwable]({
      lazy val seed = err[Int]
      lazy val random = err[Rand]
      f(seed, random)
      }) match {
      case \/-((_, r)) => r
      case -\/(e) => scalaprops.Result.Exception(IList.empty, e)
    }
    r.toMaybe.map((x: scalaprops.Result) => if(x.failed) Result.failedProp[A] else Result.satisfied(a))
      .getOrElse(Result.failedPreCond[A])
  }

  type Test[A, B] = Function2[A, Idx, B]
  type Next[A, B] = Function5[A, B, TreeForest[Boolean], Idx, List[Idx], (A, List[Idx])]

  def iter[A, B](d: A, test: Test[A, B], nxt: Next[A, B], prop: A => Property, maxLevel: Option[Int], forest: TreeForest[Boolean], idx: Idx, idxs: List[Idx])
    (implicit S: SubTypes[A]): (A, List[Idx]) = {
    val levels = breadthLevels(forest)
    lazy val done = levels.length < idx.level || tooDeep(idx.level, maxLevel)
    lazy val nextLevel = levels(idx.level).length <= idx.column
    lazy val atFalse = !(levels(idx.level)(idx.column))
    lazy val ite = iter(d, test, nxt, prop, maxLevel, forest, Idx(idx.level + 1, 0), idxs)
    if(done) (d, idxs)
    else if(nextLevel) ite
    else if(atFalse) ite
    else {
      val tries = test(d, idx)
      nxt(d, tries, forest, idx, idxs)
    }
  }
}
