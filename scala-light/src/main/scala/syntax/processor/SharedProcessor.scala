package syntax.processor

import syntax.ast.Shared.{
  Id => AstId,
  Value => AstValue,
  LiteralGroup => AstLiteralGroup,
  Argument => AstArgument,
  Reference => AstReference,
  IdReference => AstIdReference
}
import syntax.UsefulDataTypes.|
import Expression.{ Reference, MemberAccess }

object SharedProcessor {

  import Processor._
  import Shared._
  import ExpressionProcessor.processor
  import CompilationError._

  implicit val processAstId: Processor[AstId] { type ResultType = Id } = P {
    asId andThen (Result(_))
  }

  implicit val asId: AstId => Id = {
    case Left(x @ AstValue(value)) =>
      Value(value)(x)
    case Right(x @ AstLiteralGroup(literal, AstValue(value))) =>
      LiteralGroup(literal, value)(x)
  }

  implicit val processReference: Processor[AstReference] { type ResultType = Reference | MemberAccess } = P(
    _
      .map(processIdReference.process)
      .foldLeft(_ map[Reference | MemberAccess] (Left(_))) {
        case (result, reference) =>
          for {
            e <- result
            r <- reference
          } yield MemberAccess(e.merge, r)(r.ast)
      }
  )

  val processIdReference: Processor[AstIdReference] { type ResultType = Reference } = P {
    case x @ AstIdReference(to, typeArguments) =>
      for {
        newTypeArguments <- process(typeArguments)
      } yield Reference(to, newTypeArguments)(x)
  }

  implicit val processArgument: Processor[AstArgument] { type ResultType = Argument } = P {
    case x @ AstArgument(name, tpe, defaultValue) =>
      val newArgument =
        for {
          newTpe <- process(tpe)
        } yield Argument(name, newTpe)(x)

      newArgument.withErrors(defaultValue.map(NoDefaultValuesError).toSeq)
  }

}
