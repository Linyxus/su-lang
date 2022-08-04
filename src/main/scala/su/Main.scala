package su

import su.parser.tools.*
import su.parser.grammar.PathDOT
import ParserOps.*

import java.nio.file.Paths
import java.nio.file.Path

@main def main(): Unit = {
  println("This is Su (「素」) programming language :)")
  import PathDOT._


//  val u = ParseUnit.fromPath(Paths.get("pathdot_tests/lib.scala"))
//  println(u)

//  val p = spacesP >> term <* (spacesP >> eofP)
  val p = termParser
//  val s = "let x = fun(z: (x:Top) => { this => (x:x.A) => this.type }) x.z x.y in let x = new(z: Top) { A := Bot } & { a = Top } in x"
  val s =
    """
      |let lib =
      |  new(x: { Unit: Bot .. Top }) { Unit := x.unit.type } & { unit = x.unit }
      |in lib
      |""".stripMargin
//  val s = "x.A"
//  val s = "let x = new(z: Top) { A >: Bot <: Top } in x"
//  val s = "new(z: Top) { A >: Bot <: Top }"
  val res = p.runParser(ParseUnit("test", s))
  val t = res.result match {
    case ResultType.Ok(value) => value
    case ResultType.Whoops(err) => assert(false, s"$err (unit = ${res.unit})")
  }
  println(s"Original: $s")
  println(t.show)
  println(s"Remaining: ${res.unit}")

//  locally {
//    val p = fieldDefs
//    val s = "{ a: Top } & { b: Bot } & { A >: Top <: Bot }"
//    val res = p.runParser(ParseUnit("test", s))
//    val t = res.result match {
//      case ResultType.Ok(value) => value
//      case ResultType.Whoops(err) => assert(false, err)
//    }
//    println(s"Original: $s")
//    println(t.show)
//    println(s"Remaining: ${res.unit}")
//  }


//  val p = string("hello")
//  val p2 = string(",world")
//  println((p paired p2).runParser(ParseUnit("test", "hello,world123")))
//
//  val p3 = whileP(_.isLetter)
//  println(p3.runParser(ParseUnit("test", "hello,world123")))
//
//  val protoP = choices(
//    string("https"),
//    string("http"),
//  )
//  val slashP = string("://")
//  val domainPart = p3
//  val dot = string(".")
//
//  case class Url(proto: String, parts: List[String])
//
//  val urlP = (protoP <* slashP) paired domainPart paired some(dot >> domainPart) map {
//    case ((proto, firstPart), (secondPart, moreParts)) =>
//      Url(proto, firstPart :: secondPart :: moreParts.toList)
//  }
//
//  println(urlP.runParser(ParseUnit("test", "http://www.baidu.com")))
}
