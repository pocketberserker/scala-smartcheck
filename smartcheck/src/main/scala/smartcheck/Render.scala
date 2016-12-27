package smartcheck

import scalaz._
import scalaz.std.stream._
import scalaz.std.string._
import scalaz.syntax.either._

object Render {

  import DataToTree._

  private[this] val smartPrefix = "*** "

  def smartPrintLn(s: String): Unit = println(smartPrefix + s)

  def renderWithVars[A](format: Format, d: A, idxs: Replace[Idx])(implicit S: SubTypes[A]): Unit = {
    def vars(str: String): Stream[String] =
      Stream.continually(str).zip(unfold(0)(i => Some((i, i + 1))))
        .map { case (x, i) => x + i }
    def printVars(kind: String, len: Int, vs: Stream[String]): Unit =
      if(len > 0) println("forall " + kind + " " + vs.take(len).mkString(" ") + ":")
    val idxss = idxs.copy(unConstrs = idxs.unConstrs.diff(idxs.unVals))
    val constrsLen = idxss.unConstrs.length
    def constrArgs(): Unit = if(constrsLen != 0) println("  there exist arguments x̅ s.t.")
    val valsLen = idxss.unVals.length
    val valVars = vars("x")
    printVars("values", valsLen, valVars)
    constrArgs()
    val constrVars = vars("C")
    println(replaceWithVars(format, d, idxss, Replace(valVars.toList, constrVars.toList)))
    println()
  }

  type VarRepl = String \/ String

  def replaceWithVars[A](format: Format, d: A, idxs: Replace[Idx], vars: Replace[String])(implicit S: SubTypes[A]): String = {
    def remSubVars(tree: Tree[String \/ String]): Tree[String] =
      tree.rootLabel match {
        case -\/(s) => Tree.Node(s, tree.subForest.map(remSubVars))
        case \/-(s) => Tree.Node(s, Stream.empty)
      }
    def f(tree: Tree[VarRepl], t: (String, Idx)): Tree[VarRepl] = {
      val forest = getIdxForest(tree.subForest, t._2) match {
        // TODO: fix Exception type
        case None => throw new Exception("replaceWithVars1")
        case Some(n) => n.rootLabel match {
          case -\/(_) => tree.subForest
          case \/-(_) => forestReplaceChildren(tree.subForest, t._2, t._1.right)
        }
      }
      Tree.Node(tree.rootLabel, forest)
    }
    val forest = S.showForest(d)
    // TODO: fix Exception type
    val t: Tree[VarRepl] = if(forest.isEmpty) throw new Exception("replaceWithVars2") else forest.head.map(_.left)
    val zipRepl = vars.unVals.zip(idxs.unVals) ++ vars.unConstrs.zip(idxs.unConstrs)
    val strTree = remSubVars(zipRepl.foldLeft(t)(f))
    format match {
      case PrintTree => strTree.drawTree(stringInstance)
      case PrintString => stitchTree(strTree)
    }
  }

  private[this] def stitchTree(tree: Tree[String]): String = {
    def go(t: Tree[String]): String =
      if(t.subForest.isEmpty) {
        if(t.rootLabel.exists(_.isSpaceChar)) "(" + t.rootLabel + ")"
        else t.rootLabel
      }
      else "(" + stitchTree(t) + ")"
    tree.rootLabel + " " + tree.subForest.map(go).mkString(" ")
  }
}
