package su.core.cc

import Trees._

class AlphaConversion {
  private var myCount = 0

  private def freshName(baseName: String): String =
    val result = baseName ++ "$" ++ myCount.toString
    myCount += 1
    result
  end freshName

  def applyTerm(term: Term): Term = term match {
    case Term.Lambda(argName, argType, body) =>
      val newName = freshName(argName)
      val argType1 = argType
      val body1 = renameVar(argName, newName, body)
      Term.Lambda(newName, argType1, applyTerm(body1))
    case Term.Let(name, t, body) =>
      val newName = freshName(name)
      val t1 = renameVar(name, newName, t)
      val body1 = renameVar(name, newName, t)
      Term.Let(newName, applyTerm(t1), applyTerm(body1))
    case Term.LambdaT(argName, bd, body) =>
      val newName = freshName(argName)
      val bd1 = renameTypeVar(argName, newName, bd)
      val body1 = renameTypeVar(argName, newName, body)
      Term.LambdaT(newName, bd1, applyTerm(body1))
    case t => t
  }

  def apply(term: Term): Term = applyTerm(term)
}

object AlphaConversion {
  def apply(term: Term): Term =
    val converter = new AlphaConversion
    converter(term)
}

