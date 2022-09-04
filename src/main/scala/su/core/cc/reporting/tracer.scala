package su.core.cc.reporting

object tracer {
  private var myIndentLevel: Int = 0

  def apply[T](question: String)(op: => T): T = op

  def log[T](question: String)(op: => T): T = {
    val level = myIndentLevel
    myIndentLevel += 2

    val indent = List.fill(level) { "  " } mkString ""
    println(indent ++ "==> " ++ question ++ "?")

    val result = op

    println(indent ++ "<== " ++ question ++ " = " ++ result.toString)

    myIndentLevel = level
    op
  }
}

