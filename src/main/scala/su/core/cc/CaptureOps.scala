package su.core.cc

import su.core.cc.Trees.{_, given}
import su.core.cc.typer._

object CaptureOps {
  extension (c: CaptureVar) {
    def show: String = c match {
      case CaptureVar.Var(name) => name
      case CaptureVar.Universal => "*"
    }
  }

  def isInDom(cs: CaptureSet, env: Env): Boolean =
    cs.forall { c =>
      env.termVars contains (c.show)
    }
}

