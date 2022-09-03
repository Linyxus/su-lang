package su.parser.tools

import su.parser.tools.*
import ParserOps.*

object Helpers {
  def spacesP: Parser[Unit] = many(spaceP) <# ()

  def spaces1P: Parser[Unit] = some(spaceP) <# ()

  def spaced[X](p: Parser[X]): Parser[X] = p <* spacesP

  def spaced1[X](p: Parser[X]): Parser[X] = p <* spaces1P

  extension[X] (p: Parser[X]) {
    def ^~[Y](q: => Parser[Y]): Parser[(X, Y)] = spaced(p) paired q
    def ^~~[Y](q: => Parser[Y]): Parser[(X, Y)] = spaced1(p) paired q
  }

  def identifierP: Parser[String] = (charPred(_.isLetter) paired whileP(_.isLetterOrDigit)) <#> { case (ch, xs) => s"$ch$xs" }

  given Conversion[String, Parser[String]] = string
}

