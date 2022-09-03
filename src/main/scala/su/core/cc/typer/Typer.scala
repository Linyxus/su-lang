package su.core.cc.typer

import su.core.cc.Trees
import su.core.cc.CaptureOps._
import Trees.{_, given}

object Typer {
  type TyperResult = Either[String, Type]

  def typed(env: Env, tree: Term): TyperResult = tree match {
    case Term.Var(name) => typedVar(env, name)
    case lam: Term.Lambda => ???
  }

  def typedVar(env: Env, name: String): TyperResult =
    if env.termVars contains name then
      Right(CapturingType(Set(name), env.termVars(name)))
    else
      Left(f"Unknown variable ${name}")

  def typedLam(env: Env, lam: Term.Lambda): TyperResult = {
    val env1 = env.addTermVar(lam.argName, lam.argType)
    typed(env1, lam.body)
  }

  def typedLamT(env: Env, lamT: Term.LambdaT): TyperResult = {
    val env1 = env.addTypeVar(lamT.argName, lamT.upperBound)
    typed(env1, lamT.body)
  }

  def typedBox(env: Env, box: Term.Box): TyperResult =
    typedVar(env, box.varName) flatMap {
      case ct @ CapturingType(cs, tp) =>
        if isInDom(cs, env) then Right(ShapeType.Box(ct))
        else Left(f"Capture set $cs can not be boxed")
      case tp => Left(f"Variable ${box.varName} is typed as a non-capturing type $tp")
    }

  def typedApp(env: Env, x: String, y: String): TyperResult =
    typedVar(env, x) flatMap {
      case CapturingType(cs, ShapeType.Forall(argName, argType, resType)) => ???
      case ShapeType.Forall(argName, argType, resType) => ???
      case tp => Left(s"Can not type apply : $x is of type $tp, not a function")
    }
}

