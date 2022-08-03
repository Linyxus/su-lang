package su.parser.tools

import java.nio.file.{Path, Paths}

/** A parsing unit in the parser. */
case class ParseUnit(unitName: String, content: String, currentPos: Int = 0) {
  def length: Int = content.length
  def isEnd: Boolean = currentPos >= length
  def remaining: Int = content.length - currentPos
  def peek: Char = content(currentPos)

  def forwardN(n: Int): ParseUnit = ParseUnit(unitName, content, currentPos + n)

  def forward: ParseUnit = forwardN(1)

  def emitWhoops(msg: String): ParseResult[Nothing] =
    ParseResult(ResultType.Whoops(ParseError(currentPos, msg)), this)

  def emitOk[X](x: X): ParseResult[X] =
    ParseResult(ResultType.Ok(x), this)

  override def toString: String = s"ParseUnit(remaining: \"${content.substring(currentPos)}\")"
}

object ParseUnit {
  def readFile(path: Path): String = {
    val source = io.Source.fromFile(path.toFile)
    try
      source.mkString
    finally
      source.close()
  }

  def fromPath(path: Path): ParseUnit = {
    val name = path.getFileName.toString
    val content = readFile(path)

    ParseUnit(name, content)
  }
}
