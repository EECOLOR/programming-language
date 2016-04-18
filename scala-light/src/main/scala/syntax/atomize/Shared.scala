package syntax.atomize

import syntax.Processor
import syntax.processor.Shared.{
  Id => AstId,
  Argument => AstArgument,
  Value => AstValue,
  LiteralGroup => AstLiteralGroup
}

object Shared {

  import Atom._
  import Processor._

  val context = Processor.context[CompilationError]

  type Type = Expression
  val Type: Reference = "Type"

  val `__`: Id = "_"
  val ? : Reference = "?"
  val Unit: Reference = "Unit"
  type Identifier = Application
  def Identifier(s: Id): Identifier = Application("Identifier", s)(Generated)

  case class NameTypePair(name: Id, tpe: Type)
  type ExposedMember = NameTypePair
  type UnimplementedMember = NameTypePair
  type Argument = NameTypePair

  case class ConstructedArgument(a: Id, tpe: Expression)
  case class ArgumentContainer(argument: AstArgument, defaultType: Expression)

  implicit val asId: AstId => Id = {
    case Left(x @ AstValue(value)) => Value(value)(x)
    case Right(x @ AstLiteralGroup(literal, value)) => LiteralGroup(literal, value)(x)
  }

  implicit class ArgumentsEnhancements(arguments: Seq[AstArgument]) {
    def withTypeDefault(defaultType: Expression): Seq[ArgumentContainer] =
      arguments.map(ArgumentContainer(_, defaultType))
  }

 implicit class ArgumentCreator[A](a: A)(implicit toId: A => Id) {
    def withType (tpe: Expression): ConstructedArgument = ConstructedArgument(a, tpe)
  }

  implicit class FunctionTypeCreator[A](argumentType: A)(implicit toExpression: A => Expression) {
    def `=>` (returnType: Expression) = FunctionType(argumentType, returnType)(Generated)
  }

  implicit class FunctionCreator(argument: ConstructedArgument) {
    def `=>` (body: Expression) = Function(argument.a, argument.tpe, body)(Generated)
  }

  implicit class ExpressionSeqEnhancements[A](seq: Seq[A])(implicit toExpression: A => Expression) {
    def toFunctionType(returnType: Expression): Expression =
      seq.foldRight(returnType)(_ `=>` _)

    def appliedTo(target: Expression): Expression =
      seq.foldLeft(target)(Application(_, _)(Generated))
  }

  import scala.language.implicitConversions
  implicit def anyToSeq[A](a: A): Seq[A] = Seq(a)
  implicit def stringAsId(s: String): Id = Left(Value(s)(Generated))
  implicit def stringAsReference(s: String): Reference = Reference(s)(Generated)
  implicit def idAsReference[A](s: A)(implicit asId: A => Id): Reference = Reference(s)(Generated)

  object Coproduct {
    def apply(choices: Seq[Expression]) = {
      val X = "X"
      val coproductType = choices.map(_ `=>` X).toFunctionType(returnType = X)
      (X withType Type) `=>` coproductType
    }

    def selectFrom(target: Expression, options: Seq[Function], resultType: Expression) =
      (resultType +: options).foldLeft(target)(Application(_, _)(Generated))
  }

  object Id {
    def unapply(id: AstId): Option[String] =
      id.left.toOption.map(_.value)
  }
}
