package su.core.cc

object Trees {
  enum CaptureVar {
    case Var(name: String)
    case Universal
  }

  given Conversion[String, CaptureVar] with
    def apply(s: String): CaptureVar = s match {
      case "*" => CaptureVar.Universal
      case s => CaptureVar.Var(s)
    }

  type CaptureSet = Set[CaptureVar]

  enum ShapeType {
    case TypeVar(name: String)
    case Top
    case Forall(argName: String, argType: Type, resType: Type)
    case ForallT(argName: String, upperBound: ShapeType, resType: Type)
    case Box(tp: Type)
  }

  case class CapturingType(capSet: CaptureSet, tp: Type)

  type Type = ShapeType | CapturingType

  enum Term {
    case Var(name: String)
    case Lambda(argName: String, argType: Type, body: Term)
    case LambdaT(argName: String, upperBound: Type, body: Term)
    case Box(varName: String)
    case App(x: String, y: String)
    case AppT(x: String, s: Type)
    case Let(name: String, t: Term, body: Term)
    case Unbox(capSet: CaptureSet, varName: String)
  }

  def renameVar(varName: String, to: String, cs: CaptureSet): CaptureSet =
    cs.map {
      case CaptureVar.Var(name) if name == varName => CaptureVar.Var(to)
      case v => v
    }

  def renameVar(varName: String, to: String, oldName: String): String =
    if oldName == varName then to else oldName

  def renameVar(varName: String, to: String, term: Term): Term = term match {
    case Term.Var(name: String) => Term.Var(renameVar(varName, to, name))
    case Term.Lambda(argName, argType, body) if argName != varName => Term.Lambda(argName, renameVar(varName, to, argType), renameVar(varName, to, body))
    case Term.LambdaT(argName, tb, body) => Term.LambdaT(argName, renameVar(varName, to, tb), renameVar(varName, to, body))
    case Term.Box(name) => Term.Box(renameVar(varName, to, name))
    case Term.App(x, y) => Term.App(renameVar(varName, to, x), renameVar(varName, to, y))
    case Term.AppT(x, s) => Term.AppT(renameVar(varName, to, x), renameVar(varName, to, s))
    case Term.Let(x, t1, t2) => Term.Let(renameVar(varName, to, x), renameVar(varName, to, t1), renameVar(varName, to, t2))
    case Term.Unbox(cs, name) => Term.Unbox(renameVar(varName, to, cs), renameVar(varName, to, name))
    case term => term
  }

  def renameVar(varName: String, to: String, tp: Type): Type =
    tp match {
      case st: ShapeType => renameVar(varName, to, st)
      case ct: CapturingType => renameVar(varName, to, ct)
    }

  def renameVar(varName: String, to: String, tp: ShapeType): ShapeType = tp match {
    case ShapeType.Forall(argName, argType, resType) if argName != varName =>
      ShapeType.Forall(argName, renameVar(varName, to, argType), renameVar(varName, to, resType))
    case ShapeType.ForallT(argName, bound, resType) =>
      ShapeType.ForallT(argName, renameVar(varName, to, bound), renameVar(varName, to, resType))
    case ShapeType.Box(tp) =>
      ShapeType.Box(renameVar(varName, to, tp))
    case tp => tp
  }

  def renameVar(varName: String, to: String, tp: CapturingType): CapturingType =
    CapturingType(renameVar(varName, to, tp.capSet), renameVar(varName, to, tp.tp))

  def renameTypeVar(varName: String, to: String, oldName: String): String =
    renameVar(varName, to, oldName)

  def renameTypeVar(varName: String, to: String, term: Term): Term = term match {
    case Term.LambdaT(argName, tb, body) if argName != varName =>
      Term.LambdaT(argName, renameTypeVar(varName, to, tb), renameTypeVar(varName, to, body))
    case Term.Lambda(argName, argType, body) =>
      Term.Lambda(argName, renameTypeVar(varName, to, argType), renameTypeVar(varName, to, body))
    case Term.Box(name) => Term.Box(renameTypeVar(varName, to, name))
    case Term.App(x, y) => Term.App(renameTypeVar(varName, to, x), renameTypeVar(varName, to, y))
    case Term.AppT(x, s) => Term.AppT(renameTypeVar(varName, to, x), renameTypeVar(varName, to, s))
    case Term.Let(x, t1, t2) => Term.Let(renameTypeVar(varName, to, x), renameTypeVar(varName, to, t1), renameTypeVar(varName, to, t2))
    case Term.Unbox(cs, name) => Term.Unbox(cs, renameTypeVar(varName, to, name))
    case term => term
  }

  def renameTypeVar(varName: String, to: String, tp: ShapeType): ShapeType = tp match {
    case ShapeType.Forall(argName, argType, resType) =>
      ShapeType.Forall(argName, renameTypeVar(varName, to, argType), renameTypeVar(varName, to, resType))
    case ShapeType.ForallT(argName, bound, resType) if argName != varName =>
      ShapeType.ForallT(argName, renameTypeVar(varName, to, bound), renameTypeVar(varName, to, resType))
    case ShapeType.Box(tp) =>
      ShapeType.Box(renameTypeVar(varName, to, tp))
    case ShapeType.TypeVar(name) =>
      ShapeType.TypeVar(renameTypeVar(varName, to, name))
    case tp => tp
  }

  def renameTypeVar(varName: String, to: String, tp: CapturingType): CapturingType =
    CapturingType(tp.capSet, renameTypeVar(varName, to, tp.tp))

  def renameTypeVar(varName: String, to: String, tp: Type): Type = tp match {
    case st: ShapeType => renameTypeVar(varName, to, st)
    case ct: CapturingType => renameTypeVar(varName, to, ct)
  }
}

