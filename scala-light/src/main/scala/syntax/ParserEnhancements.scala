package syntax

import fastparse.all._
import fastparse.Implicits.Sequencer
import fastparse.core.ParseCtx

class ParserEnhancements(
  whitespace        : => P[Unit],
  spaces            : => P[Unit],
  spacesSingleBreak : => P[Unit]
) {

  implicit class CustomParserOperations[X, A](parser: X)(implicit x: X => Parser[A]) {

    def ` \n` [B, C](other: Parser[B])(implicit ev: Sequencer[A, B, C]) = parser ~ whitespace ~ other
    def ` `   [B, C](other: Parser[B])(implicit ev: Sequencer[A, B, C]) = parser ~ spaces ~ other
    def `  `  [B, C](other: Parser[B])(implicit ev: Sequencer[A, B, C]) = parser ~ spacesSingleBreak ~ other

    def noCommit = NoCut(parser)
    def commit[B, C](other: Parser[B])(implicit ev: Sequencer[A, B, C]) = parser ~/ other
    def commit = parser.~/

    class * extends ParserWithSeparator[A](parser, min = 0)
    object * extends *

    class + extends ParserWithSeparator(parser, min = 1)
    object + extends +

    def maybeFollowedBy[B >: A](parsers: Parser[B => B] *): Parser[B] = {
      def chainParsers(b: B): Parser[B] = {
        val initializedParsers = parsers.map(_ map (_ apply b))
        val combinedParsers = initializedParsers.reduce(_ | _)
        val chainedParsers = combinedParsers.flatMap(chainParsers)
        chainedParsers | Pass.map(_ => b)
      }

      parser.flatMap(chainParsers)
    }
  }

  trait Separator[A] {
    def separatedBy[B](separator: Parser[B]): Parser[Seq[A]]
  }

  class ParserWithSeparator[A](parser: Parser[A], min: Int)extends Parser[Seq[A]] with Separator[A] {
    def parseRec(cfg: ParseCtx, index: Int) =
      parser.rep(min).parseRec(cfg, index)

    def separatedBy[B](separator: Parser[B]): Parser[Seq[A]] =
      parser.rep(min, sep = separator)
  }
}
