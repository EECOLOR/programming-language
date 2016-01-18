package syntax

import fastparse.all._

object Whitespace {

  import ParserUtilities._

  val ` ` = P( include(" ") )

  val ` \n` = {
    val whitespace = P( include(" \n") )
    whitespace
  }

  val `\n` = {
    val actualNewLine     = P( "\n" )
    val newLineWithSpaces = P( (" ".rep ~ actualNewLine ~ " ".rep).rep(min = 1) )
    newLineWithSpaces
  }

  lazy val `  ` = {
    val spacesWithNewLine = P( (" ".rep(min = 1) ~ "\n".? ~ " ".rep) | (" ".rep ~ "\n".? ~ " ".rep(min = 1)) )
    spacesWithNewLine
  }
}
