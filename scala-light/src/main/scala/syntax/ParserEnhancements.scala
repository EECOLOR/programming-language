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

    class * extends ZeroOrMoreWithSeparator(parser)
    object * extends *

    class + extends OneOrMoreWithSeparator(parser)
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
    def separatedBy[B](separator: Parser[B]): Parser[A]
  }

  class ZeroOrMoreWithSeparator[A](parser: Parser[A]) extends Parser[Seq[A]] with Separator[Seq[A]] {
    def parseRec(cfg: ParseCtx, index: Int) =
      parser.rep().parseRec(cfg, index)

    def separatedBy[B](separator: Parser[B]): Parser[Seq[A]] =
      parser.rep(sep = separator)
  }

  class OneOrMoreWithSeparator[A](parser: Parser[A]) extends Parser[(A, Seq[A])] with Separator[(A, Seq[A])] {
    private val toPair: Seq[A] => (A, Seq[A]) = { case Seq(a, rest @ _*) => (a, rest) }

    def parseRec(cfg: ParseCtx, index: Int) =
      parser.rep(min = 1).map(toPair).parseRec(cfg, index)

    def separatedBy[B](separator: Parser[B]): Parser[(A, Seq[A])] =
      parser.rep(min = 1, sep = separator).map(toPair)
  }
}
