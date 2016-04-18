package syntax.atomize

import syntax.Empty.empty
import syntax.UsefulDataTypes.|
import syntax.Processor
import syntax.processor.{ Expression => AstExpression }
import syntax.processor.Expression.{
  Function => AstFunction,
  Block => AstBlock,
  Application => AstApplication,
  MemberAccess => AstMemberAccess,
  Reference => AstReference,
  Product => AstProduct,
  SeparatedExpressions => AstSeparatedExpressions
}

object ExpressionProcessor {

  import Atom._
  import Processor._
  import Shared._
  import Shared.context._

  import StatementProcessor.{ processor => processStatement }
  import SharedProcessor._

  implicit val processor:
    AstExpression -> Expression = P {

      case x @ AstBlock(body) =>
        for {
          newBody        <- process(body)
          normalizedBody =  newBody.foldLeft(Seq.empty[Val | Expression]) {
                              case (result, Left(vals)) =>
                                result ++ vals.map(injectLeft)
                              case (result, Right(expression)) =>
                                result :+ Right(expression)
                            }
        } yield Block(normalizedBody)(x)

      case x @ AstApplication(target, product: AstProduct) =>
        for {
          newTarget  <- process(target)
          newProduct <- process(product)
        } yield Application(newProduct, newTarget)(x)

      case x @ AstApplication(target, argument) =>
        for {
          newTarget    <- process(target)
          newArgument <- process(argument)
        } yield Application(newTarget, newArgument)(x)

      case x @ AstFunction(arguments, body) =>
        for {
          newArguments      <- process(arguments withTypeDefault ?)
          nonEmptyArguments =  if (newArguments.isEmpty) Seq(NameTypePair("_", Unit)) else newArguments
          newBody           <- process(body)
        } yield
          nonEmptyArguments.foldRight(newBody) {
            case (NameTypePair(name, tpe), result) => Function(name, tpe, result)(x)
          }

      case x @ AstReference(to, typeArguments) =>
        for {
          newTypeArguments <- process(typeArguments)
        } yield newTypeArguments.foldLeft(Reference(to)(x) : Expression)(Application(_, _)(x))

      case x @ AstMemberAccess(target, AstReference(to, typeArguments)) =>
        for {
          newTarget        <- process(target)
          newTypeArguments <- process(typeArguments)
          typedMember      =  Application(("body" withType Identifier(to)) `=>` "body", to)(to.merge)
          memberAccess     =  Application(newTarget, typedMember)(x)
        } yield newTypeArguments.foldLeft(memberAccess)(Application(_, _)(x))

      case AstProduct(expressions) =>
        for {
          newExpressions <- process(expressions)
          count          =  newExpressions.size
        } yield {
          def call(target: Expression, expressions: Seq[Expression]) =
            expressions.foldLeft(target)(Application(_, _)(Generated))
          val X = "X"
          val productType = Seq.fill(count)(?).toFunctionType(returnType = X) `=>` X
          (X withType Type) `=>` (("f" withType productType) `=>` call("f", newExpressions))
        }

      case x @ AstSeparatedExpressions(Id("=>"), expressions) =>
        for {
          newExpressions <- process(expressions)
        } yield newExpressions.foldRight(x => x)(FunctionType(_, _)(x))

      case AstSeparatedExpressions(Id("|"), expressions) =>
        for {
          newExpressions <- process(expressions)
        } yield Coproduct(newExpressions.toSeq)

      case x @ AstSeparatedExpressions(separator, expressions) =>
        for {
          newExpressions <- process(expressions)
        } yield newExpressions.foldLeft(x => x)((target, argument) => Application(Application(target, separator)(x), argument)(x))
    }
}
