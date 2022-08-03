package su.core.pathdot.typecheck

import su.core.pathdot.Trees._

case class Context(typeEnv: Map[String, Type],
                   seenPathDuringResolving: Set[String])

object Context {
  def ctx(using ctx0: Context) = ctx0

  def freshCtx: Context =
    Context(Map.empty, Set.empty)

  def withSeenPath[X](path: Term.Path)(using Context)(op: Context ?=> X): X = {
    val p0 = path.show
    val ctx1 = ctx.copy(seenPathDuringResolving = ctx.seenPathDuringResolving + p0)
    val result = op(using ctx1)
    result
  }
}
