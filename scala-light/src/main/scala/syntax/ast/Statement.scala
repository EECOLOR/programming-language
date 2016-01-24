package syntax.ast

import Shared._

sealed trait Statement
object Statement {

  case class Package(path: Seq[Id], body: Seq[Statement | Expression]) extends Statement

  case class Marked(mark: Id, statement: Statement) extends Statement

  case class Comment(comment: Indexed) extends Statement

  trait Import extends Statement
  object Import {
    case class Single(path: Reference) extends Import
    case class Multiple(path: Reference, parts: Seq[Part]) extends Import

    trait Part
    case class Id(id: Shared.Id) extends Part
    case class As(original: Shared.Id, newId: Shared.Id) extends Part
  }

  case class Trait (name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], extensions: Seq[Extension], body: Seq[Statement | Expression]) extends Statement
  case class Object(name: Id, typeArguments: Seq[Argument], extensions: Seq[Extension], body: Seq[Statement | Expression]) extends Statement
  case class Class (name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], extensions: Seq[Extension], body: Seq[Statement | Expression]) extends Statement

  case class Extension(id: Id, expression: Expression)

  case class Def(name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], `type`: Option[Expression], body: Expression) extends Statement
  case class Val(id: Id, typeArguments: Seq[Argument], `type`: Option[Expression], body: Expression) extends Statement
  case class TypeConstructor(id: Id, typeArguments: Seq[Argument], body: Expression) extends Statement
  case class UnimplementedMember(id: Id, typeArguments: Seq[Argument], arguments:Seq[Argument], `type`: Expression) extends Statement

  case class MemberExtraction(target: Option[Reference], members: Seq[Id], source: Expression) extends Statement
}
