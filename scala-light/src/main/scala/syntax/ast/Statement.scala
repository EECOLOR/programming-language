package syntax.ast

trait Statement
object Statement {

  import Core._

  case class Marked(mark: Id, statement: Statement) extends Statement

  case class Comment(comment: Indexed) extends Statement

  trait Import extends Statement
  object Import {
    case class Single(path: QualifiedReference) extends Import
    case class Multiple(path: QualifiedReference, parts: Seq[Part]) extends Import

    trait Part
    object Part {
      case class Id(id: Core.Id) extends Part
      case class As(original: Core.Id, newId: Core.Id) extends Part
    }
  }

  case class Class(name: Id, typeArguments: Option[Arguments], arguments: Arguments, extensions: Seq[Extension], body: Option[Block]) extends Statement
  case class Object(name: Id, typeArguments: Option[Arguments], extensions: Seq[Extension], body: Option[Block]) extends Statement
  case class Trait(name: Id, typeArguments: Option[Arguments], arguments: Option[Arguments], extensions: Seq[Extension], body: Option[Block]) extends Statement

  case class Def(signature: Typed[(Id, Option[Arguments], Option[Arguments])], body: Expression) extends Statement
  case class Val(id: Typed[(Id, Option[Arguments])], body: Expression) extends Statement
  case class TypeConstructor(id: Id, typeArguments: Arguments, body: Expression) extends Statement

  case class UnimplementedMember(signature: Typed[(Id, Option[Arguments], Option[Arguments])]) extends Statement

  type |[+A, +B] = Either[A, B]
}
