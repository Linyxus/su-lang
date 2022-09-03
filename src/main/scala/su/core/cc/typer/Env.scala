package su.core.cc.typer

import su.core.cc.Trees
import Trees.{_, given}

case class Env(termVars: Map[String, Type], typeVars: Map[String, Type]) {
  def addTermVar(name: String, tp: Type): Env =
    assert(!(termVars contains name))
    Env(termVars.updated(name, tp), typeVars)

  def addTypeVar(name: String, bound: Type): Env =
      assert(!(typeVars contains name))
      Env(termVars, typeVars.updated(name, bound))
}

