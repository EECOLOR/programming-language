package syntax.ast

trait Constructor[A] {
  type From
  type To = A
  val construct: From => To
}

object Constructors {
  def construct[A]: Construct[A] = new Construct[A]

  implicit def constructorTupling_1[A, B, C]    : ((A, B) => C)    => (((A, B)) => C)    = f => { case (a, b)    => f(a, b) }
  implicit def constructorTupling_2[A, B, C, D] : ((A, B, C) => D) => (((A, B, C)) => D) = f => { case (a, b, c) => f(a, b, c) }

  implicit def GenericConstructor3[A, B, C, D] = new Constructor[((A, B, C)) => D] {
    type From = (A, B, C) => D
    val construct: From => To = f => { case (a, b, c) => f(a, b, c) }
  }

  class Construct[A]
  object Construct {
    import scala.language.implicitConversions
    implicit def toConstructor[A, B](construct: Construct[A])(implicit constructor: Constructor[A] { type From = B }): B => A =
      constructor construct _
  }

  import Shared._
  import Statement._
  import Expression._

  implicit object ClassConstructor extends Constructor[Class] {
    type From = (Id, Option[Seq[Argument]], Seq[Argument], Seq[Extension], Option[Option[Seq[Statement | Expression]]])
    val construct: From => To = { case (name, typeArguments, arguments, extensions, body) =>
       Class(name, typeArguments getOrElse Seq.empty, arguments, extensions, body.flatten getOrElse Seq.empty)
    }
  }

  implicit object ObjectConstructor extends Constructor[Object] {
    type From = (Id, Option[Seq[Argument]], Seq[Extension], Option[Option[Seq[Statement | Expression]]])
    val construct: From => To = { case (name, typeArguments, extensions, body) =>
      Object(name, typeArguments getOrElse Seq.empty, extensions, body.flatten getOrElse Seq.empty)
    }
  }

  implicit object TraitConstructor extends Constructor[Trait] {
    type From = (Id, Option[Seq[Argument]], Option[Seq[Argument]], Seq[Extension], Option[Option[Seq[Statement | Expression]]])
    val construct: From => To = { case (name, typeArguments, arguments, extensions, body) =>
      Trait(name, typeArguments getOrElse Seq.empty, arguments getOrElse Seq.empty, extensions, body.flatten getOrElse Seq.empty)
    }
  }

  implicit object BlockConstructor extends Constructor[Block] {
    type From = Option[Seq[Statement | Expression]]
    val construct: From => To = { case (body) =>
      Block(body getOrElse Seq.empty)
    }
  }

  implicit object FunctionConstructor extends Constructor[Function] {
    type From = (Seq[Argument] | Id, Expression)
    val construct: From => To = { case (arguments, expression) =>
      Function(ArgumentsConstructor construct arguments, expression)
    }
  }

  implicit object BlockFunctionConstructor extends Constructor[BlockFunction] {
    type From = (Seq[Argument] | Id, Seq[Statement | Expression])
    val construct: From => To = { case (arguments, body) =>
      BlockFunction(ArgumentsConstructor construct arguments, Block(body))
    }
  }

  implicit object ArgumentConstructor extends Constructor[Seq[Argument]] {
    type From = Id
    val construct: From => To = id => Seq(Argument(id, None, None))
  }

  implicit object ArgumentsConstructor extends Constructor[Seq[Argument]] {
    type From = Seq[Argument] | Id
    val construct: From => To = _.right.map(ArgumentConstructor.construct).merge
  }

  implicit object WhitespaceApplicationConstructor extends Constructor[Expression => WhitespaceApplication] {
    type From = (IdReference, Expression)
    val construct: From => To = { case (method, argument) =>
      target => WhitespaceApplication(target, method, argument)
    }
  }

  implicit object ProductApplicationConstructor extends Constructor[Expression => ProductApplication] {
    type From = Seq[Expression]
    val construct: From => To = { case (arguments) =>
      target => ProductApplication(target, arguments)
    }
  }

  implicit object NamedProductApplicationConstructor extends Constructor[Expression => NamedProductApplication] {
    type From = Seq[(Option[Id], Expression)]
    val construct: From => To = { case (arguments) =>
      target => NamedProductApplication(target, arguments)
    }
  }

  implicit def ExpressionApplicationConstructor[A <: Expression] = new Constructor[Expression => Application] {
    type From = A
    val construct: From => To = { case (expression) =>
      target => Application(target, expression)
    }
  }

  implicit object MemberAccessConstructor extends Constructor[Expression => MemberAccess] {
    type From = IdReference
    val construct: From => To = { case (member) =>
      target => MemberAccess(target, member)
    }
  }

  implicit object PackageConstructor extends Constructor[Package] {
    type From = (Option[Seq[Id]], Seq[Statement | Expression])
    val construct: From => To = { case (path, body) =>
      Package(path getOrElse Seq.empty, body)
    }
  }

  implicit object IdReferenceConstructor extends Constructor[IdReference] {
    type From = (Id, Option[Seq[Expression]])
    val construct: From => To = { case (id, typeApplication) =>
      IdReference(id, typeApplication getOrElse Seq.empty)
    }
  }

  implicit object DefConstructor extends Constructor[Def] {
    type From = (Id, Option[Seq[Argument]], Option[Seq[Argument]], Option[Expression], Expression)
    val construct: From => To = { case (id, typeArguments, arguments, tpe, body) =>
      Def(id, typeArguments getOrElse Seq.empty, arguments getOrElse Seq.empty, tpe, body)
    }
  }

  implicit object ValConstructor extends Constructor[Val] {
    type From = (Id, Option[Seq[Argument]], Option[Expression], Expression)
    val construct: From => To = { case (id, typeArguments, tpe, body) =>
      Val(id, typeArguments getOrElse Seq.empty, tpe, body)
    }
  }

  implicit object TypeConstructorConstructor extends Constructor[TypeConstructor] {
    type From = (Id, Option[Seq[Argument]], Expression)
    val construct: From => To = { case (id, typeArguments, body) =>
      TypeConstructor(id, typeArguments getOrElse Seq.empty, body)
    }
  }

  implicit object UnimplementedMemberConstructor extends Constructor[UnimplementedMember] {
    type From = (Id, Option[Seq[Argument]], Option[Seq[Argument]], Expression)
    val construct: From => To = { case (id, typeArguments, arguments, tpe) =>
      UnimplementedMember(id, typeArguments getOrElse Seq.empty, arguments getOrElse Seq.empty, tpe)
    }
  }
}
