package su.core.cc.typer

import su.core.cc.Trees._
import su.core.cc.CaptureOps._
import su.core.cc.reporting.tracer

object SubtypeChecker {
  def isSubCapture(env: Env, cs1: CaptureSet, cs2: CaptureSet): Boolean = tracer.log(s"isSubCapture $cs1 <:< $cs2") {
    def captureSetOf(varName: String): CaptureSet =
      assert(env.termVars contains varName)
      env.termVars(varName) match {
        case CapturingType(cs, _) => cs
        case _: ShapeType => Set.empty
      }

    def isElemOf(cv: CaptureVar, cs: CaptureSet): Boolean =
      (cs contains cv) || cv.match {
        case CaptureVar.Var(name) => isSubCapture(env, captureSetOf(name), cs)
        case _ => false
      }

    cs1.forall(isElemOf(_, cs2))
  }

  def isSubType(env: Env, tp1: Type, tp2: Type): Boolean = tracer.log(s"isSubType $tp1 <:< $tp2") {
    def compareTypeVar(name1: String) =
      assert(env.typeVars contains name1)
      val hi = env.typeVars(name1)
      isSubType(env, hi, tp2)

    def compareFun(arg1: String, tp1: Type, res1: Type, arg2: String, tp2: Type, res2: Type) =
      def compareArg = isSubType(env, tp2, tp1)
      def compareRes = {
        val res11 = renameVar(arg1, arg2, res1)
        val env1 = env.addTermVar(arg2, tp2)
        isSubType(env1, res11, res2)
      }
      compareArg && compareRes

    def compareTypeFun(arg1: String, bound1: Type, res1: Type, arg2: String, bound2: Type, res2: Type) =
      def compareBounds = isSubType(env, bound2, bound1)
      def compareRes = {
        val res11 = renameTypeVar(arg1, arg2, res1)
        val env1 = env.addTypeVar(arg2, tp2)
        isSubType(env1, res11, res2)
      }
      compareBounds && compareRes

    if tp1 == tp2 then true
    else (tp1, tp2) match {
      case (ShapeType.TypeVar(name), tp2) => compareTypeVar(name)
      case (_, ShapeType.Top) => true
      case (ShapeType.Forall(arg1, tp1, res1), ShapeType.Forall(arg2, tp2, res2)) =>
        compareFun(arg1, tp1, res1, arg2, tp2, res2)
      case (ShapeType.ForallT(arg1, bound1, res1), ShapeType.ForallT(arg2, bound2, res2)) =>
        compareTypeFun(arg1, bound1, res1, arg2, bound2, res2)
      case (CapturingType(cs1, tp1), CapturingType(cs2, tp2)) =>
        isSubCapture(env, cs1, cs2) && isSubType(env, tp1, tp2)
      case (CapturingType(cs1, tp1), tp2) =>
        isSubCapture(env, cs1, Set.empty) && isSubType(env, tp1, tp2)
      case (tp1, CapturingType(cs2, tp2)) =>
        isSubCapture(env, Set.empty, cs2) && isSubType(env, tp1, tp2)
      case (ShapeType.Box(tp1), ShapeType.Box(tp2)) =>
        isSubType(env, tp1, tp2)
      case _ => false
    }
  }
}

