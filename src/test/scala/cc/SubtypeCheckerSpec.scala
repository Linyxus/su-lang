package cc

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import su.core.cc._
import Trees.{_, given}
import CaptureOps._
import su.core.cc.typer._


class SubtypeCheckerSpec extends AnyFlatSpec with should.Matchers {
  "Subtype checker" should s"check subcapture relationship" in {
    val env0 = Env(
      Map(
        "a" -> CapturingType(Set("*"), ShapeType.Top),
        "b" -> CapturingType(Set("*"), ShapeType.Top),
        "c" -> CapturingType(Set("a", "b"), ShapeType.Top),
      ),
      Map.empty
    )

    SubtypeChecker.isSubCapture(env0, Set("a"), Set("*")) should be (true)
    SubtypeChecker.isSubCapture(env0, Set("*"), Set("a")) should be (false)
    SubtypeChecker.isSubCapture(env0, Set("a"), Set("a", "b")) should be (true)
    SubtypeChecker.isSubCapture(env0, Set("a"), Set("b")) should be (false)
    SubtypeChecker.isSubCapture(env0, Set("c"), Set("a", "b")) should be (true)
    SubtypeChecker.isSubCapture(env0, Set("c"), Set("a")) should be (false)
  }
}
