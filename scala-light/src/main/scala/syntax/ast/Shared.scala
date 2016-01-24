package syntax.ast

object Shared {
  type |[+A, +B] = Either[A, B]

  type Id = Indexed | LiteralGroup
  type Reference = Seq[IdReference]

  case class Argument(id: Id, `type`: Option[Expression], defaultValue: Option[Expression])
  case class IdReference(to: Id, typeApplication: Seq[Expression])
  case class Indexed(index: Int, value: String)
  case class LiteralGroup(literal: String, value: Indexed)
}
