package su.core.pathdot.typer

import su.core.pathdot.Trees.{Term, Type}
import Context.ctx

trait PathLookup {
  def lookup(path: Term.Path)(using Context): Type = {
    ???
  }
}
