package syntax.ast

import syntax.UsefulDataTypes.|
import syntax.UsefulDataTypes.NonEmptySeq

abstract class Constructor[A] {
  type From
  type To = A
  val construct: From => To
}

object Constructors {
  def construct[A]: Construct[A] = new Construct[A]

  class Construct[A]
  object Construct {
    import scala.language.implicitConversions
    implicit def toConstructor[A, B](construct: Construct[A])(implicit constructor: Constructor[A] { type From = B }): B => A =
      constructor construct _
  }

  import Shared._
  import Statement._
  import Expression._

  trait Positioned[T] extends Constructor[Position => T]

  implicit def withPosition[F, T](implicit constructor: Constructor[Position => T] { type From = F }): Constructor[T] {
    type From = (F, Position)
  } = new Constructor[T] {
    type From = (F, Position)
    val construct: From => To = { case (value, position) =>
      constructor construct value apply position
    }
  }

  class Positioned1[A, T](constructor: A => Position => T) extends Positioned[T] {
    type From = A
    val construct: From => To = constructor
  }
  class Positioned2[A, B, T](constructor: (A, B) => Position => T) extends Positioned[T] {
    type From = (A, B)
    val construct: From => To = { case (a, b) => constructor(a, b) }
  }
  class Positioned3[A, B, C, T](constructor: (A, B, C) => Position => T) extends Positioned[T] {
    type From = (A, B, C)
    val construct: From => To = { case (a, b, c) => constructor(a, b, c) }
  }

  implicit def NonEmptySeqConstructor[A] = new Constructor[NonEmptySeq[A]] {
    type From = (A, Seq[A])
    val construct: From => To = { case (a, b) => NonEmptySeq(a, b) }
  }

  implicit object ClassConstructor extends Positioned[Class] {
    type From = (Id, Option[Seq[Argument]], Seq[Argument], Seq[Extension], Option[Option[Seq[Statement | Expression]]])
    val construct: From => To = { case (name, typeArguments, arguments, extensions, body) =>
       Class(name, typeArguments getOrElse Seq.empty, arguments, extensions, body.flatten getOrElse Seq.empty)
    }
  }

  implicit object ObjectConstructor extends Positioned[Object] {
    type From = (Id, Option[Seq[Argument]], Seq[Extension], Option[Option[Seq[Statement | Expression]]])
    val construct: From => To = { case (name, typeArguments, extensions, body) =>
      Object(name, typeArguments getOrElse Seq.empty, extensions, body.flatten getOrElse Seq.empty)
    }
  }

  implicit object TraitConstructor extends Positioned[Trait] {
    type From = (Id, Option[Seq[Argument]], Option[Seq[Argument]], Seq[Extension], Option[Option[Seq[Statement | Expression]]])
    val construct: From => To = { case (name, typeArguments, arguments, extensions, body) =>
      Trait(name, typeArguments getOrElse Seq.empty, arguments getOrElse Seq.empty, extensions, body.flatten getOrElse Seq.empty)
    }
  }

  implicit object BlockConstructor extends Positioned[Block] {
    type From = Option[Seq[Statement | Expression]]
    val construct: From => To = { case (body) =>
      Block(body getOrElse Seq.empty)
    }
  }

  implicit object FunctionConstructor extends Positioned[Function] {
    type From = (Seq[Argument] | Id, Expression)
    val construct: From => To = { case (arguments, expression) =>
      Function(ArgumentsConstructor construct arguments, expression)
    }
  }

  implicit object BlockFunctionConstructor extends Positioned[BlockFunction] {
    type From = (Seq[Argument] | Id, Seq[Statement | Expression])
    val construct: From => To = { case (arguments, body) =>
      BlockFunction(ArgumentsConstructor construct arguments, body)
    }
  }

  implicit object IdArgumentConstructor extends Constructor[Seq[Argument]] {
    type From = Id
    val construct: From => To = id => Seq(Argument(id, None, None)(id.fold(_.position, _.position)))
  }

  implicit object ArgumentsConstructor extends Constructor[Seq[Argument]] {
    type From = Seq[Argument] | Id
    val construct: From => To = _.right.map(IdArgumentConstructor.construct).merge
  }

  implicit object ArgumentConstructor extends Positioned[Argument] {
    type From = (Id, Option[Expression], Option[Expression])
    val construct: From => To = { case (id, tpe, defaultValue) =>
      Argument(id, tpe, defaultValue)
    }
  }

  implicit object WhitespaceApplicationConstructor extends Positioned[Expression => WhitespaceApplication] {
    type From = (IdReference, Expression)
    val construct: From => To = { case (method, argument) =>
      position => target => WhitespaceApplication(target, method, argument)(position)
    }
  }

  implicit object ProductApplicationConstructor extends Positioned[Expression => ProductApplication] {
    type From = Product
    val construct: From => To = { case (product) =>
      position => target => ProductApplication(target, product)(position)
    }
  }

  implicit object NamedProductApplicationConstructor extends Positioned[Expression => NamedProductApplication] {
    type From = ((Option[Id], Expression), Seq[(Option[Id], Expression)])
    val construct: From => To = { case (head, tail) =>
      position => target => NamedProductApplication(target, NonEmptySeq(head, tail))(position)
    }
  }

  implicit def ExpressionApplicationConstructor[A <: Expression] = new Positioned[Expression => Application] {
    type From = A
    val construct: From => To = { case (expression) =>
      position => target => Application(target, expression)(position)
    }
  }

  implicit object MemberAccessConstructor extends Positioned[Expression => MemberAccess] {
    type From = IdReference
    val construct: From => To = { case (member) =>
      position => target => MemberAccess(target, member)(position)
    }
  }

  implicit object PackageConstructor extends Positioned[Package] {
    type From = (Option[(Id, Seq[Id])], Seq[Statement | Expression])
    val construct: From => To = { case (path, body) =>
      Package(path.fold(Seq.empty[Id]) { case (head, tail) => head +: tail }, body)
    }
  }

  implicit object IdReferenceConstructor extends Positioned[IdReference] {
    type From = (Id, Option[(Expression, Seq[Expression])])
    val construct: From => To = { case (id, typeApplication) =>
      IdReference(id, typeApplication map { case (head, tail) => head +: tail } getOrElse Seq.empty)
    }
  }

  implicit object DefConstructor extends Positioned[Def] {
    type From = (Id, Option[Seq[Argument]], Option[Seq[Argument]], Option[Expression], Expression)
    val construct: From => To = { case (id, typeArguments, arguments, tpe, body) =>
      Def(id, typeArguments getOrElse Seq.empty, arguments getOrElse Seq.empty, tpe, body)
    }
  }

  implicit object ValConstructor extends Positioned[Val] {
    type From = (Id, Option[Seq[Argument]], Option[Expression], Expression)
    val construct: From => To = { case (id, typeArguments, tpe, body) =>
      Val(id, typeArguments getOrElse Seq.empty, tpe, body)
    }
  }

  implicit object TypeConstructorConstructor extends Positioned[TypeConstructor] {
    type From = (Id, Option[Seq[Argument]], Expression)
    val construct: From => To = { case (id, typeArguments, body) =>
      TypeConstructor(id, typeArguments getOrElse Seq.empty, body)
    }
  }

  implicit object UnimplementedMemberConstructor extends Positioned[UnimplementedMember] {
    type From = (Id, Option[Seq[Argument]], Option[Seq[Argument]], Expression)
    val construct: From => To = { case (id, typeArguments, arguments, tpe) =>
      UnimplementedMember(id, typeArguments getOrElse Seq.empty, arguments getOrElse Seq.empty, tpe)
    }
  }

  implicit object ReferenceConstructor extends Constructor[Shared.Reference] {
    type From = (IdReference, Seq[IdReference])
    val construct: From => To = { case (first, rest) => Shared.Reference(NonEmptySeq(first, rest)) }
  }

  implicit object ByNameConstructor              extends Positioned1(ByName.apply)
  implicit object ValueConstructor               extends Positioned1(Value.apply)
  implicit object LiteralGroupConstructor        extends Positioned2(LiteralGroup.apply)
  implicit object MemberExtractionConstructor    extends Positioned3(MemberExtraction.apply)
  implicit object CommentConstructor             extends Positioned1(Comment.apply)
  implicit object ImportIdConstructor            extends Positioned1(Import.Id.apply)
  implicit object ProductConstructor             extends Positioned1(Product.apply)
  implicit object ReferenceExpressionConstructor extends Positioned1(Expression.Reference.apply)
  implicit object MarkedLiteralGroupConstructor  extends Positioned2(MarkedLiteralGroup.apply)
  implicit object ImportAsConstructor            extends Positioned2(Import.As.apply)
  implicit object MarkedConstructor              extends Positioned2(Marked.apply)
  implicit object ImportConstructor              extends Positioned1(Import.apply)
  implicit object ImportSingleConstructor        extends Positioned1(Import.Single.apply)
  implicit object ImportMultipleConstructor      extends Positioned2(Import.Multiple.apply)
  implicit object ExtensionConstructor           extends Positioned2(Extension.apply)
}
