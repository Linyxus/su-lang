package su.parser.tools

/** Parsing result. */
case class ParseResult[+X](result: ResultType[X], unit: ParseUnit) {
  def map[Y](func: X => Y): ParseResult[Y] = result match
    case ResultType.Ok(value) => ParseResult(ResultType.Ok(func(value)), unit)
    case ResultType.Whoops(err) => ParseResult(ResultType.Whoops(err), unit)
}

/** Type of the parsing result.
 *
 * Can either be successful with a result, or error. */
enum ResultType[+X]:
  case Ok[X](value: X) extends ResultType[X]
  case Whoops(err: ParseError) extends ResultType[Nothing]
