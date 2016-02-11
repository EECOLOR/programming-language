package syntax.processor

import syntax.ast.{ Statement => AstStatement }
import syntax.ast.{ Expression => AstExpression }
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
  Marked => AstMarkedStatement,
  Import => AstImport,
  Comment => AstComment
}
import syntax.ast.Shared.{ Argument => AstArgument, Reference => AstReference }
import syntax.UsefulDataTypes.|
import syntax.UsefulDataTypes.NonEmptySeq

object StatementProcessor {

  import Shared.Argument
  import Processor._
  import Statement._
  import SharedProcessor._
  import CompilationError._
  import ExpressionProcessor.{ processor => expressionProcessor }

  implicit val processor: Processor[AstStatement] { type ResultType = Statement } = P {
    case x @ AstPackage(path, body) =>
      for {
        newBody <- withoutUnimplementedMember(body)
        newPath <- process(path)
      } yield newPath match {
        case None =>
          RootPackage(newBody)(x)

        case Some(path) =>
          Package(path, newBody)(x)
      }

    case x @ AstObject(name, typeArguments, extensions, body) =>
      val `object` =
        for {
          newBody  <- withoutUnimplementedMember(body)
          newTypeArguments <- process(typeArguments)
        } yield Object(name, newTypeArguments, newBody)(x)

      `object`.withErrors(extensions.map(NoExtensionsError))

    case x @ AstClass(name, typeArguments, arguments, extensions, body) =>
      val `class` =
        for {
          newTypeArguments <- process(typeArguments)
          unimplementedMembers <- argumentsAsUnimplementedMembers(arguments)
          newBody <- withoutUnimplementedMember(body)
        } yield Class(name, newTypeArguments, unimplementedMembers, newBody)(x)

      `class`.withErrors(extensions.map(NoExtensionsError))

    case x @ AstTrait(name, typeArguments, arguments, extensions, body) =>
      val `trait` =
        for {
          newTypeArguments <- process(typeArguments)
          newArguments     <- process(arguments)
          (unimplementedMembers, newBody) <- separateUnimplementedMembers(body)
        } yield Trait(name, newTypeArguments, newArguments, unimplementedMembers, newBody)(x)

      `trait`.withErrors(extensions.map(NoExtensionsError))

    case x @ AstDef(name, typeArguments, arguments, tpe, body) =>
      for {
        processedTypeArguments <- process(typeArguments)
        processedArguments <- process(arguments)
        newTpe <- process(tpe)
        newBody <- process(body)
      } yield Def(name, processedTypeArguments, processedArguments, newTpe, newBody)(x)

    case x @ AstVal(name, typeArguments, tpe, body) =>
      for {
        processedTypeArguments <- process(typeArguments)
        newTpe <- process(tpe)
        newBody <- process(body)
      } yield Val(name, processedTypeArguments, newTpe, newBody)(x)

    case x @ AstTypeConstructor(name, typeArguments, body) =>
      for {
        processedTypeArguments <- process(typeArguments)
        newBody <- process(body)
      } yield TypeConstructor(name, processedTypeArguments, newBody)(x)

    case x @ AstMemberExtraction(target, names, expression) =>
      for {
        newTarget <- process(target)
        newExpression <- process(expression)
      } yield MemberExtraction(newTarget, names map asId, newExpression)(x)

    case x @ AstMarkedStatement(mark, statement) =>
      for {
        newStatement <- process(statement)
      } yield MarkedStatement(mark, newStatement)(x)

    case x @ AstImport(Right(single)) =>
      for {
        reference <- process(single.path)
      } yield Import(reference)(x)

    case x @ AstImport(Left(multiple)) =>
      for {
        imports <- process(multiple)
      } yield Statements(imports.toSeq.map(_.merge))(x)

    case x @ AstComment(_) =>
      Result(Statements(empty)(x))

    case x: AstUnimplementedMember =>
      Result(Statements(empty)(x), UnexpectedUnimplementedMember(x))
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

  private implicit val multipleImportsProcessor: Processor[AstImport.Multiple] { type ResultType = NonEmptySeq[ImportAs | Import] } = P {
    case AstImport.Multiple(base, parts) =>
      val imports = parts.map((base, _))

      for {
        newImports <- process(imports)
      } yield newImports
  }

  private implicit val importsProcessor: Processor[(AstReference, AstImport.As | AstImport.Id)] { type ResultType = ImportAs | Import } = P {
    case (base, Left(x @ AstImport.As(idRef, id))) =>
      for {
        newRef <- process(base :+ idRef)
        newId  <- process(id)
      } yield ImportAs(newRef, newId)(x)
    case (base, Right(x @ AstImport.Id(idRef))) =>
      for {
        newRef <- process(base :+ idRef)
      } yield Import(newRef)(x)
  }

  private def argumentsAsUnimplementedMembers(arguments: Seq[AstArgument]) =
    for {
      processedArguments <- seqProcessor(argumentAsUnimplementedMember) process arguments
    } yield processedArguments.flatten

  private def argumentAsUnimplementedMember: Processor[AstArgument] { type ResultType = Option[UnimplementedMember] } = P {
    case x @ AstArgument(_, Some(tpe), _) =>
      for {
        Argument(name, Some(tpe)) <- process(x)
      } yield Some(UnimplementedMember(name, typeArguments = empty, arguments = empty, tpe)(x))
    case x =>
      Result(ArgumentWithoutTypeAsUnimplmentedMemberError(x))
  }

  private def separateUnimplementedMembers(body: Seq[AstStatement | AstExpression]): Result[(Seq[UnimplementedMember], Seq[Statement | Expression])] = {
    body.foldLeft(Result((Seq.empty[UnimplementedMember], Seq.empty[Statement | Expression]), empty)) {
      case (result, Left(x @ AstUnimplementedMember(name, typeArguments, arguments, tpe))) =>
        for {
          (unimplementedMembers, body) <- result
          processedTypeArguments       <- process(typeArguments)
          processedArguments           <- process(arguments)
          newTpe                       <- process(tpe)
          newUnimplementedMember =
            UnimplementedMember(name, processedTypeArguments, processedArguments, newTpe)(x)
        } yield (unimplementedMembers :+ newUnimplementedMember, body)
      case (result, x) =>
        for {
          (unimplementedMembers, body) <- result
          element                      <- process(x)
        } yield (unimplementedMembers, body :+ element)
    }
  }
}
