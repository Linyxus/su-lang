package cc

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import su.core.cc._
import Trees._

class RenameSpec extends AnyFlatSpec with should.Matchers {
  "Renamer" should s"rename variable names correctly" in {
    renameVar("a", "b", "a") should be ("b")
    renameVar("a", "b", "c") should be ("c")
  }

  it should "rename terms correctly" in {
    import Term._
    val tests = List(
      Var("a") -> Var("b"),
      Var("b") -> Var("b"),
      Var("c") -> Var("c"),
      Lambda("a", ShapeType.Top, Var("a")) -> Lambda("a", ShapeType.Top, Var("a")),
      Lambda("b", ShapeType.Top, Var("a")) -> Lambda("b", ShapeType.Top, Var("b")),
      LambdaT("a", ShapeType.Top, Var("a")) -> LambdaT("a", ShapeType.Top, Var("b"))
    )

    for (i, o) <- tests do {
      renameVar("a", "b", i) should be (o)
    }
  }

  it should "rename types correctly" in {
    import ShapeType._
    val tests: List[(Type, Type)] = List(
      TypeVar("a") -> TypeVar("a"),
      Forall("a", Top, Top) -> Forall("a", Top, Top),
      CapturingType(Set("a", "*"), Top) -> CapturingType(Set("b", "*"), Top),
      Forall("b", CapturingType(Set("a"), Top), Top) -> Forall("b", CapturingType(Set("b"), Top), Top),
      Forall("a", CapturingType(Set("a"), Top), Top) -> Forall("a", CapturingType(Set("a"), Top), Top),
    )

    for (i, o) <- tests do {
      renameVar("a", "b", i) should be (o)
    }
  }
}

