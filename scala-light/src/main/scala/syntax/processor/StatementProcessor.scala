package syntax.processor

import syntax.Empty.empty
import syntax.Processor
import syntax.Result
import syntax.UsefulDataTypes.|
import syntax.UsefulDataTypes.NonEmptySeq
import syntax.ast.{ Expression => AstExpression }
import syntax.ast.{  Statement => AstStatement  }
import syntax.ast.Statement.{
                Class => AstClass,
              Comment => AstComment,
                  Def => AstDef,
            Extension => AstExtension,
               Import => AstImport,
               Marked => AstMarkedStatement,
     MemberExtraction => AstMemberExtraction,
               Object => AstObject,
              Package => AstPackage,
                Trait => AstTrait,
      TypeConstructor => AstTypeConstructor,
  UnimplementedMember => AstUnimplementedMember,
                  Val => AstVal
}
import syntax.ast.Shared.{
   Argument => AstArgument,
  Reference => AstReference,
      Value => AstValue
}

object StatementProcessor {

  import CompilationError._
  import ExpressionProcessor.{ processor => expressionProcessor }
  import Processor._
  import Shared.Argument
  import Shared.context._
  import SharedProcessor._
  import Statement._

  import scala.language.implicitConversions
  implicit private def injectStatements(x: Statement): Seq[Statement] = Seq(x)

  implicit val processor:
    AstStatement -> Seq[Statement] = P {
      case x @ AstPackage(Seq(), body) =>
        for {
          newBody <- withoutUnimplementedMember(body)
        } yield newBody

      case x @ AstPackage(Seq(first, rest @ _*), body) =>
        for {
          newBody  <- withoutUnimplementedMember(body)
          newFirst <- process(first)
          newRest  <- process(rest)
        } yield newRest.foldRight(Object(newFirst, typeArguments = empty, newBody)(x)) {
          case (id, body) => Object(id, typeArguments = empty, Seq(body))(x)
        }

      case x @ AstObject(name, typeArguments, extensions, body) =>
        for {
          newBody          <- withoutUnimplementedMember(body)
          newTypeArguments <- process(typeArguments)
                              .withErrors(extensions.map(NoExtensionsError))
        } yield Object(name, newTypeArguments, newBody)(x)

      case x @ AstClass(name, typeArguments, arguments, extensions, body) =>
        for {
          newTypeArguments     <- process(typeArguments)
          unimplementedMembers <- argumentsAsUnimplementedMembers(arguments)
          newBody              <- withoutUnimplementedMember(body)
                                  .withErrors(extensions.map(NoExtensionsError))
        } yield Class(name, newTypeArguments, unimplementedMembers, newBody)(x)

      case x @ AstTrait(name, typeArguments, arguments, extensions, body) =>
        for {
          newTypeArguments                <- process(typeArguments)
          newArguments                    <- process(arguments)
          (unimplementedMembers, newBody) <- separateUnimplementedMembersFrom(body)
                                             .withErrors(extensions.map(NoExtensionsError))
        } yield Trait(name, newTypeArguments, newArguments, unimplementedMembers, newBody)(x)

      case x @ AstDef(name, typeArguments, arguments, tpe, body) =>
        for {
          processedTypeArguments <- process(typeArguments)
          processedArguments     <- process(arguments)
          newTpe <- process(tpe)
          newBody <- process(body)
        } yield Def(name, processedTypeArguments, processedArguments, newTpe, newBody)(x)

      case x @ AstVal(name, typeArguments, tpe, body) =>
        for {
          processedTypeArguments <- process(typeArguments)
          newTpe                 <- process(tpe)
          newBody                <- process(body)
        } yield Val(name, processedTypeArguments, newTpe, newBody)(x)

      case x @ AstTypeConstructor(name, typeArguments, body) =>
        for {
          processedTypeArguments <- process(typeArguments)
          newBody                <- process(body)
        } yield TypeConstructor(name, processedTypeArguments, newBody)(x)

      case x @ AstMemberExtraction(target, names, expression) =>
        Result(MemberExtractionNotSupportedError(x))

      case x @ AstMarkedStatement(mark, statement) =>
        for {
          newStatements <- process(statement)
        } yield newStatements.map(x => MarkedStatement(mark, x)(x.ast))

      case x @ AstImport(Right(single)) =>
        for {
          reference <- process(single.path)
          name      =  reference.fold(_.to, _.member.to)
        } yield Import(reference, name)(x)

      case x @ AstImport(Left(multiple)) =>
        for {
          imports <- process(multiple)
        } yield imports.toSeq

      case x @ AstComment(_) =>
        Result(empty)

      case x: AstUnimplementedMember =>
        Result(empty, UnexpectedUnimplementedMember(x))
    }

  private implicit val multipleImportsProcessor:
    AstImport.Multiple -> NonEmptySeq[Import] = P {
      case AstImport.Multiple(base, parts) =>
        val imports = parts.map((base, _))

        for {
          newImports <- process(imports)
        } yield newImports
    }

  private implicit val importsProcessor:
    (AstReference, AstImport.As | AstImport.Id) -> Import = P {

      case (base, Left(x @ AstImport.As(idRef, id))) =>
        for {
          newRef <- process(base :+ idRef)
          newId  <- process(id)
        } yield Import(newRef, newId)(x)

      case (base, Right(x @ AstImport.Id(idRef))) =>
        for {
          newRef <- process(base :+ idRef)
        } yield Import(newRef, idRef.to)(x)
    }

  private def argumentAsUnimplementedMember:
    AstArgument -> Option[UnimplementedMember] = P {
      case x @ AstArgument(_, Some(tpe), _) =>
        for {
          Argument(name, Some(tpe)) <- process(x)
        } yield Some(UnimplementedMember(name, tpe)(x))
      case x =>
        Result(ArgumentWithoutTypeAsUnimplmentedMemberError(x))
    }

  private def argumentsAsUnimplementedMembers(arguments: Seq[AstArgument]) =
    for {
      processedArguments <- seqProcessor(argumentAsUnimplementedMember) process arguments
    } yield processedArguments.flatten

  private def withoutUnimplementedMember(body: Seq[AstStatement | AstExpression]) =
    body.foldLeft(Result(Seq.empty[Statement], Seq.empty[CompilationError])) {
      case (result, Left(x: AstUnimplementedMember)) =>
        result withError NoUnimplementedMembersInObjectError(x)
      case (result, Right(x)) =>
        result withError UnexpectedExpressionInStatementPosition(x)
      case (result, Left(x)) =>
        for {
          body      <- result
          processed <- process(x)
        } yield body ++ processed
    }

  private def separateUnimplementedMembersFrom(body: Seq[AstStatement | AstExpression]) =
    body.foldLeft(Result((Seq.empty[UnimplementedMember], Seq.empty[Statement]), Seq.empty[CompilationError])) {
      case (result, Left(x @ AstUnimplementedMember(name, typeArguments, arguments, tpe))) =>
        for {
          (unimplementedMembers, body) <- result
            .withErrors(typeArguments map TypeArgumentsNotSupportedError)
            .withErrors(    arguments map ArgumentsNotSupportedError    )
          newTpe                       <- process(tpe)
          newUnimplementedMember = UnimplementedMember(name, newTpe)(x)
        } yield (unimplementedMembers :+ newUnimplementedMember, body)
      case (result, Right(x)) =>
        result withError UnexpectedExpressionInStatementPosition(x)
      case (result, Left(x)) =>
        for {
          (unimplementedMembers, body) <- result
          elements                     <- process(x)
        } yield (unimplementedMembers, body ++ elements)
    }
}
