package syntax

import fastparse.all._
import fastparse.ParserApi

object ParserUtilities {

  implicit def constructorAlignment_1[A, B, C]      : ((A, B) => C)       => ((A, B))       => C = f => { case (a, b)       => f(a, b) }
  implicit def constructorAlignment_2[A, B, C, D]   : ((A, B, C) => D)    => ((A, B, C))    => D = f => { case (a, b, c)    => f(a, b, c) }
  implicit def constructorAlignment_3[A, B, C, D, E]: ((A, B, C, D) => E) => ((A, B, C, D)) => E = f => { case (a, b, c, d) => f(a, b, c, d) }
  implicit def constructorAlignment_4[A, B, C, D]   : ((A, B, C) => D)    => (A, (B, C))    => D = f => { case (a, (b, c))  => f(a, b, c) }

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
