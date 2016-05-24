package syntax.ast

import syntax.UsefulDataTypes.|
import syntax.UsefulDataTypes.NonEmptySeq
import Shared._

sealed trait AstNode {
  val position: Position
}

object Shared {
  type Id = Value | LiteralGroup
  case class Reference(to: NonEmptySeq[IdReference])

  case class Argument(id: Id, `type`: Option[Expression], defaultValue: Option[Expression])(val position: Position) extends AstNode
  case class IdReference(to: Id, typeApplication: Seq[Expression])(val position: Position) extends AstNode
  case class LiteralGroup(literal: String, value: Value)(val position: Position) extends AstNode
  case class Value(value: String)(val position: Position) extends AstNode
}

sealed trait Statement extends AstNode
object Statement {

  case class Package(path: Seq[Id], body: Seq[Statement | Expression])(val position: Position) extends Statement

  case class Marked(mark: Id, statement: Statement)(val position: Position) extends Statement

  case class Comment(comment: Value)(val position: Position) extends Statement

  case class Import(`import`: Import.Multiple | Import.Single)(val position: Position) extends Statement
  object Import {
    case class Single(path: Reference)(val position: Position) extends AstNode
    case class Multiple(path: Reference, parts: NonEmptySeq[As | Id])(val position: Position) extends AstNode

    case class Id(id: IdReference)(val position: Position) extends AstNode
    case class As(original: IdReference, newId: Shared.Id)(val position: Position) extends AstNode
  }

  case class Trait (name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], extensions: Seq[Extension], body: Seq[Statement | Expression])(val position: Position) extends Statement
  case class Object(name: Id, typeArguments: Seq[Argument], extensions: Seq[Extension], body: Seq[Statement | Expression])(val position: Position) extends Statement
  case class Class (name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], extensions: Seq[Extension], body: Seq[Statement | Expression])(val position: Position) extends Statement

  case class Extension(id: Id, expression: Expression)(val position: Position) extends AstNode

  case class Def(name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], `type`: Option[Expression], body: Expression)(val position: Position) extends Statement
  case class Val(name: Id, typeArguments: Seq[Argument], `type`: Option[Expression], body: Expression)(val position: Position) extends Statement
  case class TypeConstructor(name: Id, typeArguments: Seq[Argument], body: Expression)(val position: Position) extends Statement
  case class UnimplementedMember(id: Id, typeArguments: Seq[Argument], arguments:Seq[Argument], `type`: Expression)(val position: Position) extends Statement

  case class MemberExtraction(target: Option[Reference], names: NonEmptySeq[Id], source: Expression)(val position: Position) extends Statement
}

sealed trait Expression extends AstNode
object Expression {

  case class Application(target: Expression, argument: Expression)(val position: Position) extends Expression
  case class Block(body: Seq[Statement | Expression])(val position: Position) extends Expression
  case class BlockFunction(arguments: Seq[Argument], body: Seq[Statement | Expression])(val position: Position) extends Expression
  case class ByName(expression: Expression)(val position: Position) extends Expression
  case class Function(arguments: Seq[Argument], body: Expression)(val position: Position) extends Expression
  case class MarkedLiteralGroup(mark: Value, literalGroup: LiteralGroup)(val position: Position) extends Expression
  case class MemberAccess(target: Expression, member: IdReference)(val position: Position) extends Expression
  case class NamedExpression(name: Id, expression: Expression)(val position: Position) extends Expression
  case class NamedProductApplication(target: Expression, arguments: NonEmptySeq[NamedExpression | Expression])(val position: Position) extends Expression
  case class Product(expressions: Seq[Expression])(val position: Position) extends Expression
  case class ProductApplication(target: Expression, product: Product)(val position: Position) extends Expression
  case class Reference(to: Shared.Reference)(val position: Position) extends Expression
  case class WhitespaceApplication(target: Expression, method: IdReference, argument: Expression)(val position: Position) extends Expression
}

