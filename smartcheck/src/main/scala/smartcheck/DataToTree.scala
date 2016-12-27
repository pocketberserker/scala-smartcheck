package smartcheck

import scalaz._
import scalaz.std.stream._
import scalaz.std.anyVal._
import scalaz.TreeLoc.TreeForest

object DataToTree {

  def breadthLevels[A](forest: TreeForest[A]): Stream[Stream[A]] =
    unfold(0)(i => Some((i, i + 1)))
      .map(getLevel(forest, _)).takeWhile(!_.isEmpty)

  def depth[A](forest: TreeForest[A]): Int = {
    def go(t: Tree[A]): Int =
      if(t.subForest.isEmpty) 1
      else 1 + depth(t.subForest)
    streamInstance.maximum(forest.map(go)).getOrElse(0)
  }

  private def levelLength[A](n: Int, t: Tree[A]): Int =
    if(n == 0) t.subForest.length
    else t.subForest.map(x => levelLength(n - 1, x)).sum

  def getIdxForest[A](forest: TreeForest[A], idx: Idx): Option[Tree[A]] = {
    val l = idx.level - 1
    def findTree(n: Int, o: Option[Tree[A]]): (Int, Option[Tree[A]]) =
      o match {
        case None => (n, None)
        case Some(t) => {
          if(n < 0) (n, None)
          else {
            val len = levelLength(l, t)
            if(n < len) (n - len, getIdxForest(t.subForest, Idx(l, n)))
            else (n - len, None)
          }
        }
      }
    if(idx.level == 0) {
      if(forest.length > idx.column) forest.lift(idx.column)
      else None
    }
    else {
      streamInstance.mapAccumL(forest.map(Some(_)), idx.column)(findTree)._2
        .flatten
        .headOption
    }
  }

  private def getLevel[A](fs: TreeForest[A], n: Int): Stream[A] =
    if(n == 0) fs.map(_.rootLabel)
    else fs.flatMap((t: Tree[A]) => getLevel(t.subForest, n - 1))

  def getAtIdx[A](d: A, idx: Idx, maxDepth: Option[Int])(implicit S: SubTypes[A]): Option[SubT] =
    if(tooDeep(idx.level, maxDepth)) None
    else {
      val lev = getLevel(S.subTypes(d), idx.level)
      if(lev.length > idx.column) lev.lift(idx.column)
      else None
    }

  def tooDeep(l: Int, o: Option[Int]): Boolean =
    o.map(l > _).getOrElse(false)

  private[this] sealed abstract class SubStrat
  private[this] case object Parent extends SubStrat
  private[this] case object Children extends SubStrat

  private[this] def sub[A](strat: SubStrat, forest: TreeForest[A], idx: Idx, a: A): TreeForest[A] = {
    def f(i: Int, node: Tree[A]): (Int, Tree[A]) =
      if(i == idx.column) {
        val news = strat match {
          case Parent => Tree.Node(a, Stream.empty)
          case Children => forest(idx.column).map(Function.const(a))
        }
        (i + 1, news)
      }
      else (i + 1, node)
    def findTree(n: Int, t: Tree[A]): (Int, Tree[A]) = {
      val l = idx.level - 1
      val len = levelLength(l, t)
      if(n < 0) (n, t)
      else if(n < len) {
        val newRootLabel = strat match {
          case Parent => a
          case Children => t.rootLabel
        }
        val newTree = Tree.Node(newRootLabel, sub(strat, t.subForest, Idx(l, n), a))
        (n - len, newTree)
      }
      else (n - len, t)
    }
    if(idx.level == 0) streamInstance.mapAccumL(forest, 0)(f)._2
    else streamInstance.mapAccumL(forest, idx.column)(findTree)._2
  }

  private def forestReplaceParent[A](forest: TreeForest[A], idx: Idx, a: A): TreeForest[A] =
    sub(Parent, forest, idx, a)

  def forestReplaceChildren[A](forest: TreeForest[A], idx: Idx, a: A): TreeForest[A] =
    sub(Children, forest, idx, a)

  def mkSubstForest[A, B](a: A, b: B)(implicit S: SubTypes[A]): TreeForest[B] =
    S.subTypes(a).map(_.map(Function.const(b)))

  def replaceAtIdx[A, B](m: A, idx: Idx, b: B)(implicit S: SubTypes[A]): Option[A] =
    S.replaceChild(m, forestReplaceParent(mkSubstForest(m, Keep), idx, SubstC), b)
}
