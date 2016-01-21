package syntax

import fastparse.all._

object Separators {

  import ParserUtilities._

  val ` ` = P( include(" ") )

  val ` \n` = {
    val whitespace = P( include(" \n") )
    whitespace
  }

  val `\n` = {
    val newLineWithSpaces = P( (" ".rep ~ "\n" ~ " ".rep).rep(min = 1) )
    newLineWithSpaces
  }

  val `  ` = {
    val spacesWithNewLine = P( (" ".rep(min = 1) ~ "\n".? ~ " ".rep) | (" ".rep ~ "\n".? ~ " ".rep(min = 1)) )
    spacesWithNewLine
  }

  val `,` =
    P( ` \n`.? ~ "," ~/ ` \n` )
}
