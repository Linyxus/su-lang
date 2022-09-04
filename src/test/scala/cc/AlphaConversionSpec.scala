package cc

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import su.core.cc._
import Trees._

class AlphaConversionSpec extends AnyFlatSpec with should.Matchers {
  "Alpha conversion" should "convert terms correctly" in {
    import Term._
    AlphaConversion(Lambda("a", ShapeType.Top, Var("a"))) should be (Lambda("a$0", ShapeType.Top, Var("a$0")))
    AlphaConversion(Lambda("a", ShapeType.Top, Lambda("a", CapturingType(Set("a"), ShapeType.Top), Var("a")))) should be (
      Lambda("a$0", ShapeType.Top, Lambda("a$1", CapturingType(Set("a"), ShapeType.Top), Var("a$1")))
    )
  }
}

