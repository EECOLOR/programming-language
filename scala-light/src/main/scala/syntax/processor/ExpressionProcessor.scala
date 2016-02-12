package syntax.processor

import syntax.ast.AstNode
import syntax.ast.{ Expression => AstExpression }
import syntax.ast.Expression.{
              Application => AstApplication,
                    Block => AstBlock,
            BlockFunction => AstBlockFunction,
                 Function => AstFunction,
             MemberAccess => AstMemberAccess,
  NamedProductApplication => AstNamedProductApplication,
                  Product => AstProduct,
       ProductApplication => AstProductApplication,
                Reference => AstReferenceExpression,
    WhitespaceApplication => AstWhitespaceApplication
}
import syntax.ast.Shared.{
           Id => AstId,
  IdReference => AstIdReference
}
import syntax.UsefulDataTypes.|

object ExpressionProcessor {

  import Shared.Id
  import Processor._
  import Expression._
  import SharedProcessor._
  import StatementProcessor.{ processor => statementProcessor }

  implicit val processor:
    Processor[AstExpression] { type ResultType = Expression } = P {
      case x @ AstReferenceExpression(to) =>
        for {
          reference <- process(to)
        } yield reference.merge

      case x @ AstBlock(body) =>
        for {
          newBody <- process(body)
        } yield asSingleExpression(newBody)(x)

      case x @ AstApplication(target, argument) =>
        for {
          newTarget   <- process(target)
          newArgument <- process(argument)
        } yield Application(newTarget, newArgument)(x)

      case x @ AstFunction(arguments, body) =>
        for {
          newArguments <- process(arguments)
          newBody      <- process(body)
        } yield Function(newArguments, newBody)(x)

      case x @ AstBlockFunction(arguments, body) =>
        for {
          newArguments <- process(arguments)
          newBody      <- process(body)
        } yield Function(newArguments, asSingleExpression(newBody)(x))(x)

      case x @ AstMemberAccess(target, member) =>
        for {
          newTarget <- process(target)
          newMember <- processIdReference.process(member)
        } yield MemberAccess(newTarget, newMember)(x)

      case x @ AstWhitespaceApplication(_, AstIdReference(id, Seq()), _) =>
        for {
          expressions <- process(findSeparatedExpressions(id, x))
        } yield SeparatedExpressions(id, expressions)(x)

      case x @ AstWhitespaceApplication(target, member, argument) =>
        for {
          newTarget   <- process(target)
          newMember    <- processIdReference.process(member)
          newArgument <- process(argument)
        } yield Application(MemberAccess(newTarget, newMember)(x), newArgument)(x)

      case x @ AstProduct(expressions) =>
        for {
          newExpressions <- process(expressions)
        } yield Product(newExpressions)(x)

      case x @ AstProductApplication(target, product) =>
        for {
          newTarget  <- process(target)
          newProduct <- process(product)
        } yield Application(newTarget, newProduct)(x)

      case x @ AstNamedProductApplication(target, arguments) =>
        for {
          newTarget    <- process(target)
          newArguments <- seqProcessor(processNamedArguments) process arguments
        } yield NamedProductApplication(newTarget, newArguments)(x)
    }

  private implicit val processNamedArguments:
    Processor[(Option[AstId], AstExpression)] { type ResultType = (Option[Id], Expression) } = P {
      case (id, expression) =>
        for {
          newExpression <- process(expression)
        } yield (id map asId, newExpression)
    }

  private def findSeparatedExpressions(id: AstId, target: AstExpression): Seq[AstExpression] ={
    target match {
      case AstExpression.WhitespaceApplication(newTarget, AstIdReference(targetId, Seq()), argument) if targetId == id =>
        findSeparatedExpressions(id, newTarget) :+ argument
      case _ => Seq(target)
    }
  }

  private def asSingleExpression(expressions: Seq[Statement | Expression])(ast: AstNode): Expression =
    expressions match {
      case Seq(Right(expression)) => expression
      case x => Block(x)(ast)
    }
}
