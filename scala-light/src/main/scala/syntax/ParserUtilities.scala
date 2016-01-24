package syntax

import fastparse.all._
import fastparse.ParserApi

object ParserUtilities {

  object AlternativeParserBehavior {

    implicit class OrToEither[A](a: Parser[A]) {
      def | [B](b: Parser[B]): Parser[Either[A, B]] =
        originalApi(a.map(Left(_))) | b.map(Right(_))

      private def originalApi[T](p: Parser[T]): ParserApi[T] = p
    }
  }

  def include(chars: String) =
    P( CharsWhile(NamedFunction(chars.contains(_), ("include(\"" + chars + "\")").replace("\n", "\\n"))) )

  def not(chars: String) =
    P( CharsWhile(NamedFunction(!chars.contains(_), s"not($chars)")).! )

  private case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
    def apply(t: T) = f(t)
    override def toString() = name
  }
}
