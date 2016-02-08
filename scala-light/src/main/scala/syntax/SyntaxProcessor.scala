package syntax

import syntax.ast.Shared.|
import syntax.ast.AstNode
import syntax.ast.{
  Statement => AstStatement,
  Expression => AstExpression
}
import syntax.ast.Statement.{
  Package => AstPackage,
  UnimplementedMember => AstUnimplementedMember,
  Object => AstObject,
  Class => AstClass,
  Trait => AstTrait,
  Extension => AstExtension,
  Def => AstDef,
  Val => AstVal,
  TypeConstructor => AstTypeConstructor,
  MemberExtraction => AstMemberExtraction,
  Marked => AstMarkedStatement
}
import syntax.ast.Shared.{
  Id => AstId,
  Value => AstValue,
  LiteralGroup => AstLiteralGroup,
  Argument => AstArgument,
  Reference => AstReference,
  IdReference => AstIdReference
}
import syntax.ast.Expression.{
  Reference => AstReferenceExpression,
  Block => AstBlock,
  Application => AstApplication,
  Function => AstFunction,
  BlockFunction => AstBlockFunction,
  MemberAccess => AstMemberAccess,
  WhitespaceApplication => AstWhitespaceApplication
}
import sun.reflect.generics.tree.TypeArgument

object SyntaxProcessor {

  import Processor._

  def process[A](ast: A)(implicit processor: Processor[A]): Result[processor.Result] =
    processor process ast

  trait Processor[-A] {
    type Result
    val process: A => Processor.Result[Result]
  }
  object Processor extends LowerPriorityProcessors {
    case class Result[A](val value: A, val errors: Seq[CompilationError]) {
      def map[B](f: A => B): Result[B] = Result(f(value), errors)
      def flatMap[B](f: A => Result[B]): Result[B] = {
        val result = f(value)
        Result(result.value, errors ++ result.errors)
      }
      def withError(error: CompilationError): Result[A] =
        Result(value, errors :+ error)

      def withErrors(newErrors: Seq[CompilationError]): Result[A] =
        Result(value, errors ++ newErrors)

      // for member extraction
      def withFilter(f: A => Boolean): Result[A] = this
    }
    object Result {
      def apply[A](value: A): Result[A] = Result(value, empty)
      def apply[A](error: CompilationError)(implicit empty: Empty[A]): Result[A] = Result(empty.value, Seq(error))

      object Value {
        def unapply[A](r: Result[A]): Option[A] =
          Option(r) map (_.value)
      }
    }
  }
  trait LowerPriorityProcessors {
    implicit def pairProcessor[A, B](implicit left: Processor[A], right: Processor[B]): Processor[A | B] { type Result = left.Result | right.Result } =
    P(_.fold(x => left process x map (Left(_)), x => right process x map (Right(_))))

    implicit def optionProcessor[A](implicit processor: Processor[A]): Processor[Option[A]] { type Result = Option[processor.Result] } =
      P(_.map(processor process _ map (Option(_))).getOrElse(Result(None, empty)))

    implicit def seqProcessor[A](implicit processor: Processor[A]): Processor[Seq[A]] { type Result = Seq[processor.Result] } =
      P(_.map(processor.process).foldLeft(Result(Seq.empty[processor.Result], empty)) {
        (result, element) =>
          for {
            seq <- result
            e <- element
          } yield seq :+ e
      })
  }

  private def P[A, B](f: A => Result[B]): Processor[A] { type Result = B } =
    new Processor[A] {
      type Result = B
      val process = f
    }

  private implicit def injectStatement(x: Statement): Statement | Expression = Left(x)
  private implicit def injectExpression(x: Expression): Statement | Expression = Right(x)

//TODO with AST
  implicit val astStatementProcessor: Processor[AstStatement] { type Result = Statement } = P {
    case AstPackage(path, body) =>
      for {
        newBody <- withoutUnimplementedMember(body)
      } yield path match {
        case Seq() =>
          RootPackage(newBody)

        case Seq(first, rest @ _*) =>
          rest.foldRight[Statement](
            Trait(first, empty, empty, empty, newBody))(
            (id, body) => Trait(id, empty, empty, empty, Seq(body))
          )
      }
//Maybe object and class should not yet translate to trait, that seems to be semantics
//instead of restricting the AST
    case AstObject(name, typeArguments, extensions, body) =>
      for {
        newBody  <- withoutUnimplementedMember(body)
        newTrait <- `trait`(name, typeArguments, arguments = Seq.empty, extensions, unimplementedMembers = empty, newBody)
      } yield newTrait

    case AstClass(name, typeArguments, arguments, extensions, body) =>
      for {
        unimplementedMembers <- argumentsAsUnimplementedMembers(arguments)
        newBody <- withoutUnimplementedMember(body)
        newTrait <- `trait`(name, typeArguments, arguments = Seq.empty, extensions, unimplementedMembers, newBody)
      } yield newTrait

    case AstTrait(name, typeArguments, arguments, extensions, body) =>
      for {
        (unimplementedMembers, newBody) <- separateUnimplementedMembers(body)
        newTrait <- `trait`(name, typeArguments, arguments, extensions, unimplementedMembers, newBody)
      } yield newTrait

    case AstDef(name, typeArguments, arguments, tpe, body) =>
      for {
        (processedTypeArguments, processedArguments) <- processArguments(typeArguments, arguments)
        newTpe <- process(tpe)
        newBody <- process(body)
      } yield Def(name, processedTypeArguments, processedArguments, newTpe, newBody)

    case AstVal(name, typeArguments, tpe, body) =>
      for {
        processedTypeArguments <- processArguments(typeArguments)
        newTpe <- process(tpe)
        newBody <- process(body)
      } yield Val(name, processedTypeArguments, newTpe, newBody)

    case AstTypeConstructor(name, typeArguments, body: Expression) =>
      for {
        processedTypeArguments <- processArguments(typeArguments)
        newBody <- process(body)
      } yield TypeConstructor(name, processedTypeArguments, newBody)

    case AstMemberExtraction(target, names, expression) =>
      for {
        newTarget <- process(target)
        newExpression <- process(expression)
      } yield MemberExtraction(newTarget.flatten, names map asId, newExpression)

    case AstMarkedStatement(mark, statement) =>
      for {
        newStatement <- process(statement)
      } yield MarkedStatement(mark, newStatement)
  }

  implicit val astExpressionProcessor: Processor[AstExpression] { type Result = Expression } = P {
    case x @ AstReferenceExpression(to) =>
      for {
        reference <- process(to)
        result <- reference match {
          case Some(reference) => Result[Expression](reference.merge)
          case None => Result[Expression](EmptyReferenceError(x))
        }
      } yield result

    case AstBlock(body) =>
      for {
        newBody <- process(body)
      } yield asSingleExpression(newBody)

    case AstApplication(target, argument) =>
      for {
        newTarget <- process(target)
        newArgument <- process(argument)
      } yield Application(newTarget, newArgument)

    case AstFunction(arguments, body) =>
      for {
        newArguments <- processArguments(arguments)
        newBody <- process(body)
      } yield Function(newArguments, newBody)

    case AstBlockFunction(arguments, body) =>
      for {
        newArguments <- processArguments(arguments)
        newBody <- process(body)
      } yield Function(newArguments, asSingleExpression(newBody))

    case AstMemberAccess(target, member) =>
      for {
        newTarget <- process(target)
        newMember <- processIdReference.process(member)
      } yield MemberAccess(newTarget, newMember)
  }

  implicit val processReference: Processor[AstReference] { type Result = Option[Reference | MemberAccess] } = P(
    _
      .map(processIdReference.process)
      .foldLeft(Result[Option[Reference | MemberAccess]](None)) {
        case (result, reference) =>
          result.flatMap {
            case None => reference.map(x => Some(Left(x)))
            case Some(e) => reference.map(x => Some(Right(MemberAccess(e.merge, x))))
          }
      }
  )

  private val processIdReference: Processor[AstIdReference] { type Result = Reference } = P {
    case AstIdReference(to, typeArguments) =>
      for {
        newTypeArguments <- process(typeArguments)
      } yield Reference(to, newTypeArguments)
  }

  private def separateUnimplementedMembers(body: Seq[AstStatement | AstExpression]): Result[(Seq[UnimplementedMember], Seq[Statement | Expression])] = {
    body.foldLeft(Result((Seq.empty[UnimplementedMember], Seq.empty[Statement | Expression]), empty)) {
      case (result, Left(AstUnimplementedMember(name, typeArguments, arguments, tpe))) =>
        for {
          (unimplementedMembers, body) <- result
          processedTypeArguments       <- processArguments(typeArguments)
          processedArguments           <- processArguments(arguments)
          newTpe                       <- process(tpe)
          newUnimplementedMember =
            UnimplementedMember(name, processedTypeArguments, processedArguments, newTpe)
        } yield (unimplementedMembers :+ newUnimplementedMember, body)
      case (result, x) =>
        for {
          (unimplementedMembers, body) <- result
          element                      <- process(x)
        } yield (unimplementedMembers, body :+ element)
    }
  }

  private def withoutUnimplementedMember(body: Seq[AstStatement | AstExpression]): Result[Seq[Statement | Expression]] = {
      body.foldLeft(Result(Seq.empty[Statement | Expression], empty)) {
        case (result, Left(x: AstUnimplementedMember)) =>
          result withError NoUnimplementedMembersInObjectError(x)
        case (result, x) =>
          for {
            body      <- result
            processed <- process(x)
          } yield body :+ processed
      }
  }

  private def argumentsAsUnimplementedMembers(arguments: Seq[AstArgument]) =
    for {
      processedArguments <- process(arguments)
    } yield processedArguments.flatten

  private implicit def argumentAsUnimplementedMember: Processor[AstArgument] { type Result = Option[UnimplementedMember] } = P {
    case x @ AstArgument(_, Some(tpe), _) =>
      for {
        Argument(name, Some(tpe)) <- processArgument(x)
      } yield Some(UnimplementedMember(name, typeArguments = empty, arguments = empty, tpe))
    case x =>
      Result(ArgumentWithoutTypeAsUnimplmentedMemberError(x))
  }

  private def asSingleExpression(expressions: Seq[Statement | Expression]): Expression =
    expressions match {
      case Seq(Right(expression)) => expression
      case x => Block(x)
    }

  private def `trait`(
    name: AstId,
    typeArguments: Seq[AstArgument],
    arguments: Seq[AstArgument],
    extensions: Seq[AstExtension],
    unimplementedMembers: Seq[UnimplementedMember],
    body: Seq[Statement | Expression]
  ) = {
    val `trait` =
      for {
        processedTypeArguments <- processArguments(typeArguments)
        processedArguments     <- processArguments(arguments)
      } yield Trait(name, processedTypeArguments, processedArguments, unimplementedMembers, body)

    `trait`.withErrors(extensions.map(NoExtensionsError))
  }

  type Id = String | LiteralGroup
  case class LiteralGroup(literal: String, value: String)

  trait Statement
  trait Expression

  // Only allow unimplementedMember in trait
  case class Trait(name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], unimplementedMembers: Seq[UnimplementedMember], body: Seq[Statement | Expression]) extends Statement
  case class MarkedStatement(mark: Id, statement: Statement) extends Statement
  case class Block(body: Seq[Statement | Expression]) extends Expression
  case class Def(name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], `type`: Option[Expression], body: Expression) extends Statement
  case class Val(name: Id, typeArguments: Seq[Argument], `type`: Option[Expression], body: Expression) extends Statement
  case class TypeConstructor(name: Id, typeArguments: Seq[Argument], body: Expression) extends Statement
  case class Reference(to: Id, typeArguments: Seq[Expression]) extends Expression
  case class MemberExtraction(target: Option[Reference | MemberAccess], names: Seq[Id], expression: Expression) extends Statement
  case class Application(target: Expression, argument: Expression) extends Expression
  case class MemberAccess(target: Expression, member: Reference) extends Expression
  case class Function(arguments: Seq[Argument], body: Expression) extends Expression

  case class Argument(name: Id, `type`: Option[Expression])
  case class UnimplementedMember(name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], `type`: Expression)

  case class RootPackage(body: Seq[Statement | Expression]) extends Statement

  case class CompilationError(message: String, ast: Seq[AstNode])
  object CompilationError extends ((String, Seq[AstNode]) => CompilationError) {
    def apply(message: String, ast: AstNode): CompilationError = CompilationError(message, Seq(ast))
  }

  private implicit val asId: AstId => Id = {
    case Left(AstValue(value)) =>
      Left(value)
    case Right(AstLiteralGroup(literal, AstValue(value))) =>
      Right(LiteralGroup(literal, value))
  }

  private def processArguments(arguments1: Seq[AstArgument], arguments2: Seq[AstArgument]): Result[(Seq[Argument], Seq[Argument])] =
    for {
      processedArguments1 <- processArguments(arguments1)
      processedArguments2 <- processArguments(arguments2)
    } yield (processedArguments1, processedArguments2)

  private def processArguments(arguments: Seq[AstArgument]) =
    arguments.foldLeft(Result(Seq.empty[Argument])) {
      case (arguments, argument) =>
        for {
          newArguments <- arguments
          newArgument <- processArgument(argument)
        } yield newArguments :+ newArgument
    }

  private def processArgument: AstArgument => Result[Argument] = {
    case AstArgument(name, tpe, defaultValue) =>
      val newArgument =
        for {
          newTpe <- process(tpe)
        } yield Argument(name, newTpe)

      newArgument.withErrors(defaultValue.map(NoDefaultValuesError).toSeq)
  }

  private def empty[A](implicit empty: Empty[A]): A = empty.value

  private class Empty[A](val value: A)
  private object Empty extends LowerPriorityEmpty {
    implicit def forSeq[A]: Empty[Seq[A]] = new Empty(Seq.empty)
  }
  private trait LowerPriorityEmpty {
    implicit def forOption[A]: Empty[Option[A]] = new Empty(None)
    implicit def forExpression: Empty[Expression] = new Empty(Block(empty[Seq[Nothing]]))
  }

  def NoExtensionsError(x: AstNode) =
    CompilationError("Extensions are not supported yet", x)

  def NoDefaultValuesError(x: AstNode) =
    CompilationError("Default values are not supported yet", x)

  def NoUnimplementedMembersInObjectError(x: AstNode) =
    CompilationError("Objects can not have unimplemented members", x)

  def EmptyReferenceError(x: AstNode) =
    CompilationError("Invalid AST, found an empty reference", x)

  def ArgumentWithoutTypeAsUnimplmentedMemberError(x: AstNode) =
    CompilationError("Argument, intended as member, should have a type", x)
}
