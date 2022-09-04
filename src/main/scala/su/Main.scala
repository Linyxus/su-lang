package su

import java.nio.file.Paths
import java.nio.file.Path

import su.core.cc._
import Trees._
import typer._

@main def main(): Unit = {
  println("This is Su (「素」) programming language :)")

  val env =
    Env.empty
      .addTypeVar("Cap", ShapeType.Top)
      .addTypeVar("Unit", ShapeType.Top)
  val tp1 = ShapeType.Forall("x", ShapeType.TypeVar("Cap"), CapturingType(Set("x"), ShapeType.TypeVar("Unit")))
  val tp2 = ShapeType.Forall("y", ShapeType.TypeVar("Cap"), CapturingType(Set("y"), ShapeType.Top))
  val result = SubtypeChecker.isSubType(env, tp1, tp2)
}
