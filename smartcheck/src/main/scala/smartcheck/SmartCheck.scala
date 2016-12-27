package smartcheck

import shapeless._
import scalaprops._
import Render._
import ConstructorGen._
import Extrapolate._

object SmartCheck {

  def run[A: SubTypes : Gen, P: AsProperty, C <: Coproduct, K <: HList](args: ScArgs)(scProp: A => P)(implicit
    gen: LabelledGeneric.Aux[A, C],
    keys: ops.union.Keys.Aux[C, K],
    toList: ops.hlist.ToTraversable.Aux[K, List, Symbol]): Unit = {
    smartCheckRun(args, runQC(args.param, scProp))
  }

  private[this] def runQC[A, P](param: Param, scProp: A => P)(implicit P: AsProperty[P], G: Gen[A]): (Option[A], A => Property) = {

    def gen(): Option[A] = Some(G.f(param.minSize, param.rand)._2)

    smartPrintLn("Finding a counterexample with Scalaprops...")
    val prop = P.asProperty _ compose scProp
    Property.NoShrink.property1(prop).check(param, () => true, _ => ()) match {
      case CheckResult.Falsified(_, _, _) =>
        (gen(), prop)
      case CheckResult.Exhausted(_, _) =>
        (gen(), prop)
      case CheckResult.PropException(_, _, _, _) =>
        (gen(), prop)
      case _ => (None, prop)
    }
  }

  private[this] def smartCheckRun[A: SubTypes, C <: Coproduct, K <: HList](
    args: ScArgs, orig: (Option[A], A => Property))(implicit
    gen: LabelledGeneric.Aux[A, C],
    keys: ops.union.Keys.Aux[C, K],
    toList: ops.hlist.ToTraversable.Aux[K, List, Symbol]): Unit = {
    println("")
    smartPrintLn("Analyzing the first argument of the property with SmartCheck...")
    smartPrintLn("(If any stage takes too long, modify SmartCheck's arguments.)")
    smartCheck(args, List(), orig._1, orig._2)
  }

  private[this] def smartCheck[A: SubTypes, C <: Coproduct, K <: HList](
    args: ScArgs,
    ds: List[(A, Replace[Idx])],
    mcex: Option[A],
    prop: A => Property)(implicit
    gen: LabelledGeneric.Aux[A, C],
    keys: ops.union.Keys.Aux[C, K],
    toList: ops.hlist.ToTraversable.Aux[K, List, Symbol]): Unit = {

    mcex match {
      case Some(cex) =>
        val d = Reduce.smartRun(args, cex, prop)
        val valIdxs = forallExtrap(args, d, prop)
        val csIdxs = existsExtrap(args, d, valIdxs, prop)
        val replIdxs = Replace(valIdxs, csIdxs)
        showExtrapOutput(args, valIdxs, csIdxs, replIdxs, d)
        smartPrintLn("Done.")
      case None => smartPrintLn("No value to smart-shrink; done.")
    }
  }

  private[this] def existsExtrap[A: SubTypes, C <: Coproduct, K <: HList](
    args: ScArgs,
    d: A,
    valIdxs: List[Idx],
    origProp: A => Property)(implicit
    gen: LabelledGeneric.Aux[A, C],
    keys: ops.union.Keys.Aux[C, K],
    toList: ops.hlist.ToTraversable.Aux[K, List, Symbol]): List[Idx] =
    if (args.runExists) constructorGen(args, d, origProp, valIdxs)
    else List()


  private[this] def forallExtrap[A: SubTypes](args: ScArgs, d: A, origProp: A => Property): List[Idx] =
    if (args.runForall) extrapolate(args, d, origProp)
    else List()

  private[this] def showExtrapOutput[A, A1: SubTypes](args: ScArgs, valIdxs: List[A], csIdxs: List[A], replIdxs: Replace[Idx], d: A1): Unit = {
    if (args.runForall || args.runExists) {
      if ((valIdxs ++ csIdxs).isEmpty) smartPrintLn("Could not extrapolate a new value.")
      else {
        println("")
        smartPrintLn("Extrapolated value:")
        renderWithVars(args.format, d, replIdxs)
      }
    }
  }
}
