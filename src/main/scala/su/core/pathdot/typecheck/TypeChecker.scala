package su.core.pathdot.typecheck

import su.core.pathdot.Trees.{Term, Type}

trait TypeChecker extends TypeOps {
  def checkTree(tree: Term)(using Context): Type = tree match
    case Term.Path(root, selections) => ???
    case Term.Let(ident, t1, t2) => ???
    case Term.Lambda(argName, argType, body) => ???
    case Term.Application(p, q) => ???
    case Term.New(selfName, selfType, defs) => ???
}
