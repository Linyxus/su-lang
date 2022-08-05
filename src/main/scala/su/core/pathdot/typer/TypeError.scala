package su.core.pathdot.typer

import su.core.pathdot.Trees._

abstract class TypeError extends Exception {
  def when: Type | Term
  def show: String
}

object TypeError {
}
