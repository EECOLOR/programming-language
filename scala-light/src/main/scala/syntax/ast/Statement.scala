package syntax.ast

import Shared._

sealed trait Statement extends AstNode
object Statement {

  case class Package(path: Seq[Id], body: Seq[Statement | Expression])(val position: Position) extends Statement

  case class Marked(mark: Id, statement: Statement)(val position: Position) extends Statement

  case class Comment(comment: Value)(val position: Position) extends Statement

  case class Import(`import`: Import.Multiple | Import.Single)(val position: Position) extends Statement
  object Import {
    case class Single(path: Reference)(val position: Position) extends AstNode
    case class Multiple(path: Reference, parts: Seq[As | Id])(val position: Position) extends AstNode

    case class Id(id: Shared.Id)(val position: Position) extends AstNode
    case class As(original: Shared.Id, newId: Shared.Id)(val position: Position) extends AstNode
  }

  case class Trait (name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], extensions: Seq[Extension], body: Seq[Statement | Expression])(val position: Position) extends Statement
  case class Object(name: Id, typeArguments: Seq[Argument], extensions: Seq[Extension], body: Seq[Statement | Expression])(val position: Position) extends Statement
  case class Class (name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], extensions: Seq[Extension], body: Seq[Statement | Expression])(val position: Position) extends Statement

  case class Extension(id: Id, expression: Expression)(val position: Position) extends AstNode

  case class Def(name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], `type`: Option[Expression], body: Expression)(val position: Position) extends Statement
  case class Val(name: Id, typeArguments: Seq[Argument], `type`: Option[Expression], body: Expression)(val position: Position) extends Statement
  case class TypeConstructor(name: Id, typeArguments: Seq[Argument], body: Expression)(val position: Position) extends Statement
  case class UnimplementedMember(id: Id, typeArguments: Seq[Argument], arguments:Seq[Argument], `type`: Expression)(val position: Position) extends Statement

  case class MemberExtraction(target: Option[Reference], names: Seq[Id], source: Expression)(val position: Position) extends Statement
}
