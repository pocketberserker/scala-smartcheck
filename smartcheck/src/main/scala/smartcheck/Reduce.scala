package smartcheck

import scalaz.TreeLoc.TreeForest
import scalaprops._

object Reduce {

  import DataToTree._
  import SmartGen._
  import Render._

  def smartRun[A](args: ScArgs, res: A, prop: A => Property)(implicit S: SubTypes[A]): A = {
    println()
    smartPrintLn("Smart Shrinking ...")
    val n = smartShrink(args, res, prop)
    smartPrintLn("Smart-shrunk value:")
    print(S.shows(n))
    n
  }

  private[this] def smartShrink[A : SubTypes](args: ScArgs, d: A, prop: A => Property): A = {

    def err[T](s: String): T = throw new Exception(s)

    def next(x: A, res: Option[A], forest: TreeForest[Boolean], idx: Idx, idxs: List[Idx]): (A, List[Idx]) =
      res match {
        case Some(y) => {
          lazy val e = err[List[Idx]]("next-idxs")
          // TODO: prop compose expectFailre
          iter(y, test, next, prop, args.scMaxDepth, mkSubstForest(y, true), Idx(0, 0), e)
        }
        case None => {
          lazy val e = err[List[Idx]]("next-idxs")
          // TODO: prop compose expectFailre
          iter(x, test, next, prop, args.scMaxDepth, forest, idx.copy(column = idx.column + 1), e)
        }
      }

    def extractAndTest(y: A): Option[A] = resultToMaybe(resultify(prop, y))

    def testHole(s: SubT): Option[A] =
      // TODO: use Typeable?
      try {
        extractAndTest(s.unSubT.asInstanceOf[A])
      } catch {
        case _: Throwable => None
      }

    def test(x: A, idx: Idx): Option[A] = {
      val vm = getAtIdx(x, idx, args.scMaxDepth)
      vm match {
        case None => err("smartShrink0")
        case Some(v) => {
          val hole = testHole(v)
          if(hole.isDefined) hole
          else {
            // TODO: prop compose expectFailre
            val r = iterateArb(x, v, idx, args.scMaxReduce, scala.math.min(subValSize(x, idx), args.scMaxSize), prop)
            resultToMaybe(r._2)
          }
        }
      }
    }

    lazy val e = err[List[Idx]]("next-idxs")
    iter(d, test, next, prop, args.scMaxDepth, mkSubstForest(d, true), Idx(0, 0), e)._1
  }

  private[this] def resultToMaybe[A](res: Result[A]): Option[A] =
    res match {
      case Result.Satisfied(n) => Some(n)
      case _ => None
    }

  private[this] def subValSize[A : SubTypes](d: A, idx: Idx): Int = {
    getIdxForest(mkSubstForest(d, true), idx)
      .map(_.subForest)
      .map(depth(_)).getOrElse(0)
  }
}
