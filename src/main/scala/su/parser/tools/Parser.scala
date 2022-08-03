package su.parser.tools

case class Parser[+X](runParser: ParseUnit => ParseResult[X]) {
  var desc: String = ""

  def map[Y](func: X => Y): Parser[Y] = Parser { u => runParser(u).map(func) }
  def flatMap[Y](mf: X => Parser[Y]): Parser[Y] = Parser.flatten(map(mf))

  def or[Y](py: => Parser[Y]): Parser[X | Y] = Parser { u => runParser(u) match
    case res @ ParseResult(result, _) => result match
      case ResultType.Ok(_) => res
      case ResultType.Whoops(_) => py.runParser(u)
  }

  def withDesc(s: String): this.type = {
    desc = s
    this
  }

  def withError(msg: String): Parser[X] = Parser { u => runParser(u) match
    case res @ ParseResult(result, u1) => result match
      case ResultType.Ok(_) => res
      case ResultType.Whoops(ParseError(pos, _)) =>
        ParseResult(ResultType.Whoops(ParseError(pos, msg)), u1)
  }

}

object Parser:
  def pure[X](x: X): Parser[X] = Parser { u => ParseResult(ResultType.Ok(x), u) }

  def fail(msg: String): Parser[Nothing] = Parser { u => u.emitWhoops(msg) }

  def flatten[X](pp: Parser[Parser[X]]): Parser[X] = Parser { u =>
    pp.runParser(u) match
      case ParseResult(result, u1) => result match
        case ResultType.Ok(p) => p.runParser(u1)
        case ResultType.Whoops(err) => ParseResult(ResultType.Whoops(err), u1)
  }
