package su.core.cc.typer

import su.core.cc.Trees._
import su.core.cc.CaptureOps._

object SubtypeChecker {
  def isSubCapture(env: Env, cs1: CaptureSet, cs2: CaptureSet): Boolean = {
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

  def isSubType(env: Env, tp1: Type, tp2: Type): Boolean =
    def compareTypeVar(name1: String) =
      assert(env.typeVars contains name1)
      val hi = env.typeVars(name1)
      isSubType(env, hi, tp2)

    if tp1 == tp2 then true
    else (tp1, tp2) match {
      case (ShapeType.TypeVar(name), tp2) => compareTypeVar(name)
      case (_, ShapeType.Top) => true
      case (ShapeType.Forall(arg1, tp1, res1), ShapeType.Forall(arg2, tp2, res2)) =>
        false
    }
}

