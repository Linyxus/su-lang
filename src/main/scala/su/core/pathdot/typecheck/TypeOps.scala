package su.core.pathdot.typecheck

import su.core.pathdot.Trees._
import Context._

trait TypeOps {
  extension (tp: Type) {
    def isBottom: Boolean = tp.isInstanceOf[Type.Bot.type]
  }
}
