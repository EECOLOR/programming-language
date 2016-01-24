package syntax

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

  implicit def _01: Printer[Seq[Id]] = p(_ map print[Id] mkString ".")
  implicit def _02: Printer[Statement] = p {
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
      "let " + print(x.name) + " [" + print(x.typeArguments) + "] = " + print(x.body)
    case x: Import.Single =>
      "import " + print(x.path)
    case x: Import.Multiple =>
      "import " + print(x.path) + ".{ " + print(x.parts) + " }"
    case x: Comment =>
      "//" + print(x.comment)
    case x: Marked =>
      "<" + print(x.mark) + "> " + print(x.statement)
    case x: MemberExtraction =>
      print(x.target) + "(" + (x.members map print[Id] mkString ", ") + ") = " + print(x.source)
    case x: UnimplementedMember =>
      print(x.id) + "[" + print(x.typeArguments) + "](" + print(x.arguments) + "): " + print(x.`type`)

  }
  implicit def _03: Printer[Expression] = p {
    case x: Product => "(" + (x.expressions map print[Expression] mkString ", ") + ")"
    case x: Application => "(" + print(x.target) + ".apply(" + print(x.argument) + "))"
    case x: Block => "{\n" + print(x.body) + "\n}"
    case x: Function => "((" + print(x.arguments) + ") => " + print(x.body) + ")"
    case x: MarkedLiteralGroup => "<" + print(x.mark) + ">" + print(x.literalGroup)
    case x: MemberAccess => "(" + print(x.expression) + "." + print(x.member) + ")"
    case x: ProductApplication => "(" + print(x.target) + ".apply(" + (x.arguments map print[(Option[Id], Expression)] mkString ", ") + "))"
    case x: Expression.Reference => print(x.to)
  }
  implicit def _04: Printer[Seq[Statement | Expression]] = p(x => "  " + (x map print[Statement | Expression] mkString "\n") replace ("\n", "\n  "))
  implicit def _05: Printer[Indexed] = p(_.value)
  implicit def _06: Printer[LiteralGroup] = p(x => x.literal + print(x.value) + x.literal)
  implicit def _07: Printer[Seq[Argument]] = p(_ map print[Argument] mkString ", ")
  implicit def _08: Printer[Argument] = p(x => print(x.id) + ": " + print(x.`type`) + print(x.defaultValue.map(" = " + print(_))))
  implicit def _09: Printer[Seq[Extension]] = p(x => " " + (x map print[Extension] mkString " "))
  implicit def _10: Printer[Extension] = p(x => print(x.id) + " " + print(x.expression))
  implicit def _11: Printer[Shared.Reference] = p(_ map print[IdReference] mkString ".")
  implicit def _12: Printer[IdReference] = p(x => print(x.to) + "[" + print(x.typeApplication map print[Expression] mkString ", ") + "]")
  implicit def _13: Printer[Seq[Import.Part]] = p(_ map print[Import.Part] mkString ", ")
  implicit def _14: Printer[Import.Part] = p {
    case x: Import.As => print(x.original) + " => " + print(x.newId)
    case x: Import.Id => print(x.id)
  }
  implicit def _15: Printer[(Option[Id], Expression)] = p {
    case (id, expression) => print(id.map(" = " + print(_))) + print(expression)
  }
}
