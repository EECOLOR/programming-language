package syntax.processor

import syntax.ast.AstNode
import syntax.UsefulDataTypes.|

trait Expression extends ProcessedAstNode
object Expression {

  import Shared._

  case class Application(target: Expression, argument: Expression)(val ast: AstNode) extends Expression
  case class Block(body: Seq[Statement | Expression])(val ast: AstNode) extends Expression
  case class Function(arguments: Seq[Argument], body: Expression)(val ast: AstNode) extends Expression
  case class MemberAccess(target: Expression, member: Reference)(val ast: AstNode) extends Expression
  case class NamedProductApplication(target: Expression, arguments: Seq[(Option[Id], Expression)])(val ast: AstNode) extends Expression
  case class Product(expressions: Seq[Expression])(val ast: AstNode) extends Expression
  case class Reference(to: Id, typeArguments: Seq[Expression])(val ast: AstNode) extends Expression
  case class SeparatedExpressions(separator: Id, expressions: Seq[Expression])(val ast: AstNode) extends Expression
}
