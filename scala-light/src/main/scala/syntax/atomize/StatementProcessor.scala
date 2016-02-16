package syntax.atomize

import syntax.Empty.empty
import syntax.Processor
import syntax.processor.ProcessedAstNode
import syntax.processor.{ Statement => AstStatement }
import syntax.processor.Statement.{
  MarkedStatement => AstMarkedStatement,
  Object => AstObject,
  Class => AstClass,
  Trait => AstTrait,
  UnimplementedMember => AstUnimplementedMember,
  Val => AstVal,
  TypeConstructor => AstTypeConstructor,
  Def => AstDef,
  Import => AstImport
}
import syntax.processor.Shared.{
  Id => AstId,
  Argument => AstArgument
}

object StatementProcessor {

  import Atom._
  import CompilationError._
  import Processor._
  import ExpressionProcessor.{ processor => expressionProcessor }
  import Shared._
  import Shared.context._
  import SharedProcessor._

  implicit val processor:
    AstStatement -> Seq[Val] = P {

      case x @ AstVal(name, typeArguments, tpe, body) =>
        for {
          newTypeArguments <- process(typeArguments withTypeDefault Type)
          newType          <- process(tpe)
          newBody          <- process(body)
          finalBody        =  bodyWithArguments(TypedBody(newBody, newType), newTypeArguments)
        } yield Val(name, finalBody)(x)

      case x @ AstDef(name, typeArguments, arguments, tpe, body) =>
        for {
          newTypeArguments  <- process(typeArguments withTypeDefault Type)
          newArguments      <- process(arguments withTypeDefault ?)
          newType           <- process(tpe)
          newBody           <- process(body)
          nonEmptyArguments =  if (newArguments.isEmpty) Seq(NameTypePair("_", Unit)) else newArguments
          allArguments      =  newTypeArguments ++ nonEmptyArguments
          finalBody         =  bodyWithArguments(TypedBody(newBody, newType), allArguments)
        } yield Val(name, finalBody)(x)

      case x @ AstTypeConstructor(name, typeArguments, body) =>
        for {
          newTypeArguments       <- process(typeArguments withTypeDefault Type)
          newBody                <- process(body)
        } yield typeConstructor(name, newTypeArguments, newBody)(x)

      case AstMarkedStatement(mark, statement) =>
        for {
          newStatements <- process(statement) withError UnsupportedMarkedStatement(mark)
        } yield newStatements

      case x @ AstObject(name, typeArguments, body) =>
        createTrait(name, typeArguments, arguments = empty, unimplementedMembers = empty, body)(x)

      case x @ AstClass(name, typeArguments, unimplementedMembers, body) =>
        createTrait(name, typeArguments, arguments = empty, unimplementedMembers, body)(x)

      case x @ AstTrait(name, typeArguments, arguments, unimplementedMembers, body) =>
        createTrait(name, typeArguments, arguments, unimplementedMembers, body)(x)

      case x @ AstImport(reference, name) =>
        for {
          newReference <- process(reference)
        } yield Val(name, newReference)(x)
    }

  private implicit val unimplementedMemberProcessor:
    AstUnimplementedMember -> UnimplementedMember = P {
      case AstUnimplementedMember(name, tpe) =>
        for {
          newName <- process(name)
          newType <- process(tpe)
        } yield NameTypePair(newName, newType)
    }

  private def createTrait(name: AstId, typeArguments: Seq[AstArgument], arguments: Seq[AstArgument], unimplementedMembers: Seq[AstUnimplementedMember], body: Seq[AstStatement])(x: ProcessedAstNode) =
    for {
      newTypeArguments          <- process(typeArguments withTypeDefault Type)
      newArguments              <- process(arguments withTypeDefault ?)
      (exposedMembers, newBody) <- selectExposedMembersFrom(body)
      newUnimplementedMembers   <- process(unimplementedMembers)
    } yield `trait`(name, newTypeArguments, newArguments, newUnimplementedMembers, exposedMembers, newBody)(x)

  private def `trait`(name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], unimplementedMembers: Seq[UnimplementedMember], exposedMembers: Seq[ExposedMember], body: Seq[Val])(x: ProcessedAstNode) = {

    val (memberNames, memberTypes, memberIdentifiers) = toNamesTypesAndIdentifiers(unimplementedMembers ++ exposedMembers)

    val traitType = {
      val typeBody = ("f" withType Coproduct(choices = memberIdentifiers)) `=>` {
        val options = (memberIdentifiers zip memberTypes) map unusedArgumentFunction
        Coproduct.selectFrom("f", options, resultType = Type)
      }

      typeConstructor(name, typeArguments, typeBody)(x)
    }

    val traitConstructor = {
      val appliedTraitType = typeArguments.map(_.name).appliedTo(name)

      val bodyResult = ("id" withType Coproduct(choices = memberIdentifiers)) `=>` {
        val options    = (memberIdentifiers zip memberNames) map unusedArgumentFunction
        val resultType = Application(appliedTraitType, "id")(Generated)
        Coproduct.selectFrom("id", options, resultType)
      }

      val completeBody = Block(body.map(injectLeft) :+ injectRight(bodyResult))(Generated)

      val allArguments =  typeArguments ++ arguments ++ unimplementedMembers
      val newBody      =  bodyWithArguments(TypedBody(completeBody, appliedTraitType), allArguments)
      Val(name, newBody)(x)
    }

    Seq(traitType, traitConstructor)
  }

  private def toNamesTypesAndIdentifiers(pairs: Seq[NameTypePair]): (Seq[Id], Seq[Type], Seq[Identifier]) =
    pairs.foldLeft((Seq.empty[Id], Seq.empty[Type], Seq.empty[Identifier])) {
    case ((names, types, identifiers), NameTypePair(name, tpe)) =>
      (names :+ name, types :+ tpe, identifiers :+ Identifier(name))
    }

  private def unusedArgumentFunction[A, B](implicit aToExpression: A => Expression, bToExpression: B => Expression): ((A, B)) => Function = {
    case (tpe, body) => (`__` withType tpe) `=>` body
  }

  private def selectExposedMembersFrom(body: Seq[AstStatement]) = {
    val exposedMembers = Seq.empty[ExposedMember]
    val newBody = Seq.empty[Val]
    body.foldLeft(Result((exposedMembers, newBody))) {
      case (result, AstMarkedStatement(Id("+"), statement)) =>
        for {
          (exposedMembers, body) <- result
          newStatements          <- process(statement)
          newExposedMembers      =  newStatements.map(_.signature)
        } yield (exposedMembers ++ newExposedMembers, body ++ newStatements)
      case (result, statement) =>
        for {
          (exposedMembers, body) <- result
          newStatements          <- process(statement)
        } yield (exposedMembers, body ++ newStatements)
    }
  }

  private implicit class ValEnhancements(v: Val) {
    def signature: NameTypePair = NameTypePair(v.name, ?)
  }

  private def typeConstructor(name: Id, typeArguments: Seq[Argument], body: Expression)(x: ProcessedAstNode) = {
    val newBody = bodyWithArguments(TypedBody(body, Type), typeArguments)
    Val(name, newBody)(x)
  }

  private def bodyWithArguments(body: TypedBody, arguments: Seq[Argument]) =
    arguments.foldRight(body.value) {
      case (NameTypePair(name, tpe), body) => Function(name, tpe, body)(Generated)
    }

  private case class TypedBody private(value: Expression)
  private object TypedBody {
    def apply(body: Expression, `type`: Expression): TypedBody =
      TypedBody(Application(("body" withType `type`) `=>` "body", body)(Generated))

    def apply(body: Expression, `type`: Option[Expression]): TypedBody = {
      TypedBody(body, `type` getOrElse ?)
    }
  }

  private type Identifier = Application
  private def Identifier(s: Id): Identifier = Application("Identifier", s)(Generated)
}
