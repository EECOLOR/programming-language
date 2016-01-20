package syntax.ast

trait Core
object Core {

  case class Argument(id: Typed[Id], defaultValue: Option[Expression]) extends Core
  case class Arguments(arguments: Seq[Argument]) extends Core
  case class Block(body: Option[Body]) extends Core
  case class Body(body: Seq[Statement | Expression]) extends Core
  case class Extension(id: Id, expression: Option[Expression]) extends Core
  case class Id(value: Indexed | LiteralGroup) extends Core
  case class Indexed(index: Int, value: String) extends Core
  case class LiteralGroup(literal: String, value: Indexed) extends Core
  case class Package(path: Option[QualifiedId], body: Body) extends Core
  case class QualifiedId(value: Seq[Id]) extends Core
  case class QualifiedReference(value: Seq[Reference]) extends Core
  case class Reference(to: Id, typeApplication: Option[TypeApplication]) extends Core
  case class TypeApplication(arguments: Seq[Expression]) extends Core
  case class Typed[A](expression: A, `type`: Option[Expression]) extends Core

  type |[+A, +B] = Either[A, B]
}
