package su.parser.grammar

import su.parser.tools.*
import ParserOps.*
import Helpers.{given, *}

trait PathDOT {
  import su.core.pathdot.Trees
  import Trees._


  def fieldP: Parser[String] = identifierP >>= { s =>
    if s == "type" then Parser.fail("Can not select `type`, which is a special keyword")
    else Parser.pure(s)
  }

  def path: Parser[Term.Path] = {
    def sel: Parser[String] = "." >> fieldP

    (identifierP paired many(sel)) >>= { case (root, sels) =>
      val keywords = Set("in", "let", "fun", "new")
      if (keywords contains root) && sels.isEmpty then
        Parser.fail("Can not use keyword as identifier")
      else
        Parser.pure { Term.Path(root, sels.toList) }
    }
  }
  path.withDesc("path")

  def term: Parser[Term] = choices(letForm, lambda, newForm, application, path).withDesc("term")

  def letForm: Parser[Term.Let] = ("let" ^~~ identifierP ^~~ "=" ^~~ term ^~~ "in" ^~~ term) <#> {
    case (((((_, x), _), t1), _), t2) => Term.Let(x, t1, t2)
  }
  letForm.withDesc("let")

  def lambda: Parser[Term.Lambda] = ("fun" ^~ "(" ^~ identifierP ^~ ":" ^~~ typ ^~")" ^~~ term) <#> {
    case ((((((_, _), argName), _), argType), _), body) => Term.Lambda(argName, argType, body)
  }
  lambda.withDesc("lambda")

  def application: Parser[Term.Application] = (path ^~~  path) <#> {
    case (p, q) => Term.Application(p, q)
  }
  application.withDesc("application")

  def newForm: Parser[Term.New] = ("new" ^~ "(" ^~ identifierP ^~ ":" ^~~ typ ^~ ")" ^~ fieldDefs) <#> {
    case ((((((_, _), selfName), _), selfType), _), ds) => Term.New(selfName, selfType, ds)
  }

  def fieldDef: Parser[Def] = termDef <|> typeDef

  def fieldDefs: Parser[List[Def]] = {
    def dp: Parser[Def] = spacesP >> ("&" ^~ fieldDef) <#> { case (_, x) => x }

    (fieldDef paired many(dp)) <#> { case (d, ds) => d :: (ds.toList)}
  }

  def termDef: Parser[Def.TermMember] = ("{" ^~ identifierP ^~ "=" ^~~ term ^~ "}") >>= {
    case ((((_, name), _), t), _) =>
      if t.isStable then Parser.pure { Def.TermMember(name, t.asInstanceOf) } else Parser.fail(s"Unstable term: ${t.show}")
  }

  def typeDef: Parser[Def.TypeMember] = ("{" ^~ identifierP ^~~ ":=" ^~~ typ ^~ "}") <#> {
    case ((((_, name), _), tp), _) => Def.TypeMember(name, tp)
  }

  def typ: Parser[Type] = {
    val p: Parser[Type] = "&" >> spacesP >> typElem <* spacesP

    (typElem ^~ many(p)) <#> { case (t0, ts) => ts.toList.foldLeft(t0)(Type.AndType(_, _)) }
  }

  def typElem: Parser[Type] =
    choices(
      "Top" <# Type.Top,
      "Bot" <# Type.Bot,
      forallTyp,
      recTyp,
      termDec,
      typeDec,
      singletonTyp,
      pdtTyp
    )
  typ.withDesc("type")

  def forallTyp: Parser[Type.Forall] = ("(" ^~ identifierP ^~ ":" ^~ typ ^~ ")" ^~ "=>" ^~ typ) <#> {
    case ((((((_, argName), _), argType), _), _), resType) => Type.Forall(argName, argType, resType)
  }

  def recTyp: Parser[Type.Rec] = ("{" ^~ identifierP ^~ "=>" ^~ typ ^~ "}") <#> {
    case ((((_, thisName), _), thisType), _) => Type.Rec(thisName, thisType)
  }

  def singletonTyp: Parser[Type.Singleton] = (path paired ".type") <#> {
    case (p, _) => Type.Singleton(p)
  }

  def pdtTyp: Parser[Type.PDT] = path >>= {
    case Term.Path(root, selections) =>
      selections match
        case xs @ (_ :: _) => Parser.pure(Type.PDT(Term.Path(root, xs.init), xs.last))
        case Nil => Parser.fail(s"Must select a type member from path.")
  }

  def termDec: Parser[Type.TermDec] = ("{" ^~ identifierP ^~ ":" ^~ typ ^~ "}") <#> {
    case ((((_, name), _), tp), _) => Type.TermDec(name, tp)
  }

  def typeDec: Parser[Type.TypeDec] = ("{" ^~ identifierP ^~ ":" ^~ typ ^~ ".." ^~ typ ^~ "}") <#> {
    case ((((((_, name), _), lo), _), hi), _) => Type.TypeDec(name, lo, hi)
  }

  def termParser: Parser[Term] = spacesP >> term <* (spacesP >> eofP)
}

object PathDOT extends PathDOT
