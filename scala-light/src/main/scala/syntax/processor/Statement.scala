package syntax.processor

import syntax.UsefulDataTypes.|
import syntax.UsefulDataTypes.NonEmptySeq
import syntax.ast.AstNode

trait Statement extends ProcessedAstNode
object Statement {

  import Shared._
  import Expression.{ Reference, MemberAccess }

  case class Package(path: Seq[Id], body: Seq[Statement])(val ast: AstNode) extends Statement

  case class MarkedStatement(mark: Id, statement: Statement)(val ast: AstNode) extends Statement

  case class Trait(name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], unimplementedMembers: Seq[UnimplementedMember], body: Seq[Statement])(val ast: AstNode) extends Statement
  case class Object(name: Id, typeArguments: Seq[Argument], body: Seq[Statement])(val ast: AstNode) extends Statement
  case class Class(name: Id, typeArguments: Seq[Argument], unimplementedMembers: Seq[UnimplementedMember], body: Seq[Statement])(val ast: AstNode) extends Statement

  case class Def(name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], `type`: Option[Expression], body: Expression)(val ast: AstNode) extends Statement
  case class Val(name: Id, typeArguments: Seq[Argument], `type`: Option[Expression], body: Expression)(val ast: AstNode) extends Statement
  case class TypeConstructor(name: Id, typeArguments: Seq[Argument], body: Expression)(val ast: AstNode) extends Statement

  case class MemberExtraction(target: Option[Reference | MemberAccess], names: NonEmptySeq[Id], expression: Expression)(val ast: AstNode) extends Statement

  case class Import(path: Reference | MemberAccess)(val ast: AstNode) extends Statement
  case class ImportAs(path: Reference | MemberAccess, as: Id)(val ast: AstNode) extends Statement

  case class Statements(statements: Seq[Statement])(val ast: AstNode) extends Statement

  case class UnimplementedMember(name: Id, typeArguments: Seq[Argument], arguments: Seq[Argument], `type`: Expression)(val ast: AstNode) extends ProcessedAstNode
}
