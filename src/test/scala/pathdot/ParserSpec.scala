package pathdot

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.nio.file.{Path, Paths}
import java.io.File

import su.parser.tools.*
import su.parser.grammar.PathDOT
import ParserOps.*

class ParserSpec extends AnyFlatSpec with should.Matchers {
  def testSources: List[File] = {
    val p = Paths.get("./tests/pathdot")
    p.toFile.listFiles.toList.filter(_.getName.endsWith(".scala"))
  }

  for testSource <- testSources do
    "The parser" should s"parse file ${testSource.getName}" in {
      val u = ParseUnit.fromPath(testSource.toPath)
      val p = PathDOT.termParser
      val res = p.runParser(u)
      res.result.isInstanceOf[ResultType.Ok[_]] should be (true)
    }
}
