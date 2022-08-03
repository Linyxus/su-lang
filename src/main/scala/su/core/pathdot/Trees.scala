package su.core.pathdot

object Trees {
  enum Term {
    case Path(root: String, selections: List[String])
    case Let(ident: String, t1: Term, t2: Term)
    case Lambda(argName: String, argType: Type, body: Term)
    case Application(p: Path, q: Path)
    case New(selfName: String, selfType: Type, defs: List[Def])
  }

  extension (t: Term) {
    def show: String = t match
      case Term.Path(root, selections) => s"$root" ++ (if selections.nonEmpty then "." ++ selections.mkString(".") else "")
      case Term.Let(ident, t1, t2) => s"let $ident = ${t1.show} in { ${t2.show} }"
      case Term.Lambda(argName, argType, body) => s"fun($argName: ${argType.show}) ${body.show}"
      case Term.Application(p, q) => s"${p.show} ${q.show}"
      case Term.New(selfName, selfType, defs) => s"new($selfName: ${selfType.show}) ${defs.show}"

    def isValue: Boolean = t match
      case _: (Term.Lambda | Term.New) => true
      case _ => false

    def isStable: Boolean = t match
      case _: Term.Path => true
      case t if t.isValue => true
      case _ => false
  }

  type Value = Term.New | Term.Lambda
  type StableTerm = Term.Path | Value

  enum Def {
    case TermMember(name: String, v: StableTerm)
    case TypeMember(name: String, tp: Type)
  }

  extension (d: Def) {
    def show: String = d match
      case Def.TermMember(name, v) => s"{ $name = ${v.show} }"
      case Def.TypeMember(name, tp) => s"{ $name := ${tp.show} }"
  }

  extension (ds: List[Def]) {
    def show: String = ds.map(_.show).mkString(" & ")
  }

  enum Type {
    case Top
    case Bot
    case Forall(argName: String, argType: Type, resType: Type)
    case Rec(selfName: String, selfType: Type)
    case Singleton(path: Term.Path)
    case PDT(path: Term.Path, selection: String)
    case TermDec(name: String, tp: Type)
    case TypeDec(name: String, lo: Type, hi: Type)
    case AndType(tp1: Type, tp2: Type)
  }

  extension (tp: Type) {
    def show: String = tp match
      case Type.Top => "Top"
      case Type.Bot => "Bot"
      case Type.Forall(argName, argType, resType) => s"forall($argName: ${argType.show}) ${resType.show}"
      case Type.Rec(selfName, selfType) => s"rec($selfName: ${selfType.show})"
      case Type.Singleton(p) => s"${p.show}.type"
      case Type.PDT(path, selection) => s"${path.show}.$selection"
      case Type.TermDec(name, tp) => s"{ $name: ${tp.show} }"
      case Type.TypeDec(name, lo, hi) => s"{ $name: ${lo.show} .. ${hi.show} }"
      case Type.AndType(tp1, tp2) => s"${tp1.show} & ${tp2.show}"
  }
}
