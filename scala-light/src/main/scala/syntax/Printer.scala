package syntax

import syntax.UsefulDataTypes.|
import syntax.UsefulDataTypes.NonEmptySeq

class Printer[-T](val print: T => String) extends (T => String) {
  def apply(t: T) = print(t)
}

object Printer extends DefaultPrinters {
  def print[T](ast: T)(implicit printer: Printer[T]): String = printer print ast
}

trait DefaultPrinters {

  import ast._
  import Expression._
  import Shared._
  import Statement._

  def print[T](ast: T)(implicit printer: Printer[T]): String

  private def p[A](p: A => String): Printer[A] = new Printer(p)

  implicit def eitherPrinter[A : Printer, B : Printer]: Printer[A | B] = p(_.fold(print[A], print[B]))
  implicit def optionPrinter[A : Printer]: Printer[Option[A]] = p(_ map print[A] getOrElse "")
  implicit def stringPrinter: Printer[String] = p(identity)

  implicit def _00: Printer[AstNode] = p {
    case x: Package =>
      "package " + print(x.path) + "\n" + print(x.body)
    case x: Trait =>
      "trait " + print(x.name) + " [" + print(x.typeArguments) + "](" + print(x.arguments) + ")" + print(x.extensions) + " {\n" + print(x.body) + "\n}"
    case x: Object =>
      "object " + print(x.name) + " [" + print(x.typeArguments) + "]" + print(x.extensions) + " {\n" + print(x.body) + "\n}"
    case x: Class =>
      "class " + print(x.name) + " [" + print(x.typeArguments) + "](" + print(x.arguments) + ")" + print(x.extensions) + " {\n" + print(x.body) + "\n}"
    case x: Def =>
      "def " + print(x.name) + " [" + print(x.typeArguments) + "](" + print(x.arguments) + ")" + print(x.`type`.map(": " + print(_))) + " = " + print(x.body)
    case x: Val =>
      "val " + print(x.name) + " [" + print(x.typeArguments) + "]" + print(x.`type`.map(": " + print(_))) + " = " + print(x.body)
    case x: TypeConstructor =>
      "type " + print(x.name) + " [" + print(x.typeArguments) + "] = " + print(x.body)
    case x: Import =>
      "import " + print(x.`import`)
    case x: Comment =>
      "//" + print(x.comment)
    case x: Marked =>
      "<" + print(x.mark) + "> " + print(x.statement)
    case x: MemberExtraction =>
      print(x.target) + "(" + (x.names.toSeq map print[Id] mkString ", ") + ") = " + print(x.source)
    case x: UnimplementedMember =>
      print(x.id) + "[" + print(x.typeArguments) + "](" + print(x.arguments) + "): " + print(x.`type`)
    case x: Product => "(" + (x.expressions map print[Expression] mkString ", ") + ")"
    case x: Application => "(" + print(x.target) + ".apply(" + print(x.argument) + "))"
    case x: Block => "{\n" + print(x.body) + "\n}"
    case x: BlockFunction => "{" + print(x.arguments) + ") => " + print(x.body) + "}"
    case x: Function => "((" + print(x.arguments) + ") => " + print(x.body) + ")"
    case x: MarkedLiteralGroup => "<" + print(x.mark) + ">" + print(x.literalGroup)
    case x: MemberAccess => "(" + print(x.target) + "." + print(x.member) + ")"
    case x: NamedProductApplication => "(" + print(x.target) + ".apply(" + (x.arguments.toSeq map print[(Option[Id], Expression)] mkString ", ") + "))"
    case x: ProductApplication => "(" + print(x.target) + ".apply" + print(x.product) + ")"
    case x: Expression.Reference => print(x.to)
    case x: WhitespaceApplication => "(" + print(x.target) + " " + print(x.method) + " " + print(x.argument) + ")"
    case x: Value => x.value
    case x: LiteralGroup => x.literal + print(x.value) + x.literal
    case x: Argument => print(x.id) + ": " + print(x.`type`) + print(x.defaultValue.map(" = " + print(_)))
    case x: Extension => print(x.id) + " " + print(x.expression)
    case x: IdReference => print(x.to) + "[" + print(x.typeApplication map print[Expression] mkString ", ") + "]"
    case x: Import.Single => print(x.path)
    case x: Import.Multiple => print(x.path) + ".{ " + print(x.parts) + " }"
    case x: Import.As => print(x.original) + " => " + print(x.newId)
    case x: Import.Id => print(x.id)
    case x: ByName => "( => " + print(x.expression) + ")"
  }

  implicit def _01: Printer[Seq[Id]] = p(_.toSeq map print[Id] mkString ".")
  implicit def _04: Printer[Seq[Statement | Expression]] = p(x => "  " + (x map print[Statement | Expression] mkString "\n") replace ("\n", "\n  "))
  implicit def _07: Printer[Seq[Argument]] = p(_ map print[Argument] mkString ", ")
  implicit def _09: Printer[Seq[Extension]] = p(x => " " + (x map print[Extension] mkString " "))
  implicit def _11: Printer[Shared.Reference] = p(_.to.toSeq map print[IdReference] mkString ".")
  implicit def _13: Printer[NonEmptySeq[Import.As | Import.Id]] = p(_.toSeq map print[Import.As | Import.Id] mkString ", ")
  implicit def _15: Printer[(Option[Id], Expression)] = p {
    case (id, expression) => print(id.map(" = " + print(_))) + print(expression)
  }
}
