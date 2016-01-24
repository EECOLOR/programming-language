package syntax

import fastparse.all._

object Separators {

  import ParserUtilities._

  val ` ` = P( space(min = 1) )

  val ` \n` = {
    val whitespace = P( include(" \n") )
    whitespace
  }

  val `\n` = {
    val newLineWithSpaces = P( (` `.? ~ "\n" ~ ` `.?).rep(min = 1) )
    newLineWithSpaces
  }

  val `  ` = {
    val spacesWithNewLine = P( (` ` ~ "\n".? ~ ` `.?) | (` `.? ~ "\n".? ~ ` `) )
    spacesWithNewLine
  }

  val `,` = {
    val comma = P( ` \n`.? ~ "," ~/ ` \n` )
    comma
  }

  private def space(min: Int) = CharsWhile(_ == ' ', min)
}
