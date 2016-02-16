package syntax.atomize

import syntax.UsefulDataTypes.|

object Atomizer {

  import Atom._

  class Printer[-A](val print: A => String)

  def print[A](x: A)(implicit printer: Printer[A]): String = printer print x

  private def P[A](print: A => String): Printer[A] = new Printer(print)

  implicit def seqPrinter[A](implicit printer: Printer[A]): Printer[Seq[A]] =
    P(_.map(printer.print).mkString("\n"))

  implicit def coproductPrinter[A, B](implicit left: Printer[A], right: Printer[B]): Printer[A | B] =
    P(_.fold(left.print, right.print))

  implicit val atomPrinter: Printer[Atom] = P {

    case Val(name, body) =>
      "val " + print(name) + " = " + print(body)

    case Reference(to) =>
      print(to)

    case Function(argumentName, argumentType, body) =>
      "{ (" + print(argumentName) + ": " + print(argumentType) + ") => " + print(body) + " }"

    case Application(target, argument) =>
      print(target) + "(" + print(argument) + ")"

    case FunctionType(argumentType, returnType) =>
      "(" + print(argumentType) + " => " + print(returnType) + ")"

    case Block(atoms) =>
      "{ " + atoms.map(print[Atom | Atom]).mkString("\n") + " }"

    case Value(x) =>
      x

    case LiteralGroup(literal, value) =>
      literal + value + literal
  }
}
