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

    class * extends Parser[Seq[A]] {
      def parseRec(cfg: ParseCtx, index: Int) =
        parser.rep.parseRec(cfg, index)

      def separatedBy[B](separator: Parser[B]): Parser[Seq[A]] =
        parser.rep(sep = separator)
    }
    object * extends *

    class + extends Parser[Seq[A]] {
      def parseRec(cfg: ParseCtx, index: Int) =
        parser.rep(min = 1).parseRec(cfg, index)

      def separatedBy[B](separator: Parser[B]): Parser[Seq[A]] =
        parser.rep(min = 1, sep = separator)
    }
    object + extends +

    def maybeFollowedBy[B >: A](parsers: B => Parser[B] *) = {

      def chainParsers(b: B): Parser[B] = {
        val initializedParsers = parsers.map(_ apply b)
        val combinedParsers = initializedParsers.reduce(_ | _)
        val chainedParsers = combinedParsers.flatMap(chainParsers)
        chainedParsers | Pass.map(_ => b)
      }

      parser.flatMap(chainParsers)
    }
  }
}
