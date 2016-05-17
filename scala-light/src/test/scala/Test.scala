

import syntax.Parser
import scala.io.Source
import java.io.File

object Test {
  val file = new File("/home/eecolor/eecolor/programming-language/scala-light/src/main/scala/syntax/Parser.scala")
  val code = Source.fromFile(file).getLines.mkString("\n")
  val result = Parser.statements.`package`.parse(code)

  def test = {
    println(result)
    println(result.asInstanceOf[fastparse.core.Parsed.Failure].extra.traced.trace)
  }
}
