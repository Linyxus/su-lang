package su.parser.tools

trait ParserOps:
  def charPred(pred: Char => Boolean, what: String = ""): Parser[Char] = Parser { u =>
    if u.isEnd then u.emitWhoops(f"Unexpected end-of-file")
    else if pred(u.peek) then u.forward.emitOk(u.peek)
    else u.emitWhoops(s"Un-fulfilled character predicate for ${u.peek} (what=$what)")
  }

  def string(str: String): Parser[String] = Parser { u =>
    val l = str.length
    if l <= u.remaining && u.content.substring(u.currentPos, u.currentPos + l) == str then
      u.forwardN(l).emitOk(str)
    else
      u.emitWhoops(s"Expecting $str")
  }

  def eofP: Parser[Unit] =
    Parser { u => if u.isEnd then u.emitOk(()) else u.emitWhoops(f"Expecting end-of-file") }

  def many[X](p: Parser[X]): Parser[LazyList[X]] =
    some_(p) <|> Parser.pure(LazyList.empty)

  def some[X](p: Parser[X]): Parser[(X, LazyList[X])] = p paired many(p)

  def some_[X](p: Parser[X]): Parser[LazyList[X]] = some(p) <#> { (x, xs) => x #:: xs }

  extension[X] (p: Parser[X]) {
    def <|>[Y] (o: => Parser[Y]): Parser[X | Y] = p or o
    def >>=[Y](f: X => Parser[Y]): Parser[Y] = p flatMap f
    def >>[Y] (o: => Parser[Y]): Parser[Y] = p >>= (_ => o)
    def <#>[Y] (f: X => Y): Parser[Y] = p map f
    def <#[Y] (y: Y): Parser[Y] = p <#> (_ => y)
    def <*[Y] (o: => Parser[Y]): Parser[X] = p >>= (x => o <# x)

    def chainWith[Y, A] (agg: (X, Y) => A)(o: => Parser[Y]): Parser[A] =
      p >>= { x => o <#> { y => agg(x, y) } }

    def paired[Y](o: => Parser[Y]): Parser[(X, Y)] = chainWith[Y, (X, Y)]((x, y) => (x, y))(o)

    def optional: Parser[Option[X]] = p.map(Some.apply) <|> Parser.pure(None)

    def what(s: String): Parser[X] = p.withError(f"Expecting $s")
  }

  def spaceP: Parser[Char] = charPred(ch => ch.isWhitespace, what = "space")

  def digitP: Parser[Char] = charPred(ch => ch.isDigit, what = "digit")

  def whileP(pred: Char => Boolean): Parser[String] = Parser { u =>
    var l = 0
    while u.currentPos + l < u.length && pred(u.content(l + u.currentPos)) do l += 1
    u.forwardN(l).emitOk(u.content.substring(u.currentPos, u.currentPos + l))
  }

  def untilP(pred: Char => Boolean): Parser[String] = whileP(ch => !pred(ch))

  def while1P(pred: Char => Boolean): Parser[String] = whileP(pred) >>= { s =>
    if s.length > 0 then Parser.pure(s)
    else Parser.fail("while1P failed")
  }

  def choices[X](ps: Parser[X]*): Parser[X] = ps.toList match
    case Nil => Parser.fail("Empty choices list")
    case p :: ps => ps.foldLeft(p)(_ <|> _)

object ParserOps extends ParserOps
