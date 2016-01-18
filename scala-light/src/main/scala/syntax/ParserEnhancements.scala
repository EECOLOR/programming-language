package syntax

import fastparse.all._
import fastparse.Implicits.Sequencer

class ParserEnhancements(
  whitespace        : => P[Unit],
  spaces            : => P[Unit],
  spacesSingleBreak : => P[Unit]
) {

  implicit class CustomParserOperations[X, A](a: X)(implicit x: X => Parser[A]) {

    def ` \n` [B, C](b: Parser[B])(implicit ev: Sequencer[A, B, C]) = a ~ whitespace ~ b
    def ` `   [B, C](b: Parser[B])(implicit ev: Sequencer[A, B, C]) = a ~ spaces ~ b
    def `  `  [B, C](b: Parser[B])(implicit ev: Sequencer[A, B, C]) = a ~ spacesSingleBreak ~ b

    def separatedBy[B](parser: Parser[B]) =
      a.rep(sep = parser)

    def maybeFollowedBy(parsers: A => Parser[A] *) =
      a.flatMap { a =>
          def initParser(a: A): Parser[A] = parsers.map(_ apply a).reduce(_ | _).flatMap(a => initParser(a) | Pass.map(_ => a))
          val result = initParser(a)

          result | Pass.map(_ => a)
        }
  }
}
