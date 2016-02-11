package syntax.processor

import syntax.UsefulDataTypes.NonEmptySeq
import syntax.UsefulDataTypes.|
import syntax.ast.AstNode
import syntax.ast.{
  Statement => AstStatement,
  Expression => AstExpression
}
import syntax.ast.Statement.{
  Package => AstPackage,
  UnimplementedMember => AstUnimplementedMember,
  Object => AstObject,
  Class => AstClass,
  Trait => AstTrait,
  Extension => AstExtension,
  Def => AstDef,
  Val => AstVal,
  TypeConstructor => AstTypeConstructor,
  MemberExtraction => AstMemberExtraction,
  Marked => AstMarkedStatement,
  Import => AstImport,
  Comment => AstComment
}
import syntax.ast.Shared.{
  Id => AstId,
  Value => AstValue,
  LiteralGroup => AstLiteralGroup,
  Argument => AstArgument,
  Reference => AstReference,
  IdReference => AstIdReference
}
import syntax.ast.Expression.{
  Reference => AstReferenceExpression,
  Block => AstBlock,
  Application => AstApplication,
  Function => AstFunction,
  BlockFunction => AstBlockFunction,
  MemberAccess => AstMemberAccess,
  WhitespaceApplication => AstWhitespaceApplication,
  Product => AstProduct,
  NamedProductApplication => AstNamedProductApplication
}

trait Processor[-A] {
  type ResultType
  val process: A => Processor.Result[ResultType]
}
object Processor extends DefaultProcessors {

  implicit def injectLeft[A, B](x: A): A | B = Left(x)
  implicit def injectRight[A, B](x: B): A | B = Right(x)
  implicit def merge[A](x: A | A): A = x.merge

  def process[A](ast: A)(implicit processor: Processor[A]): Result[processor.ResultType] =
    processor process ast

  def P[A, B](f: A => Result[B]): Processor[A] { type ResultType = B } =
    new Processor[A] {
      type ResultType = B
      val process = f
    }

  case class Result[A](val value: A, val errors: Seq[CompilationError]) {
    def map[B](f: A => B): Result[B] = Result(f(value), errors)
    def flatMap[B](f: A => Result[B]): Result[B] = {
      val result = f(value)
      Result(result.value, errors ++ result.errors)
    }
    def withError(error: CompilationError): Result[A] =
      Result(value, errors :+ error)

    def withErrors[B >: A](newErrors: Seq[CompilationError]): Result[B] =
      Result(value, errors ++ newErrors)

    // for member extraction
    def withFilter(f: A => Boolean): Result[A] = this
  }
  object Result {
    def apply[A](value: A): Result[A] = Result(value, empty)
    def apply[A](value: A, error: CompilationError): Result[A] = Result(value, Seq(error))
    def apply[A](error: CompilationError)(implicit empty: Empty[A]): Result[A] = Result(empty.value, Seq(error))

    object Value {
      def unapply[A](r: Result[A]): Option[A] =
        Option(r) map (_.value)
    }
  }

  def empty[A](implicit empty: Empty[A]): A = empty.value

  class Empty[A](val value: A)
  object Empty extends LowerPriorityEmpty {
    implicit def forSeq[A]: Empty[Seq[A]] = new Empty(Seq.empty)
  }
  trait LowerPriorityEmpty {
    implicit def forOption[A]: Empty[Option[A]] = new Empty(None)
  }
}
trait DefaultProcessors {

  import Processor._

  implicit def pairProcessor[A, B](implicit left: Processor[A], right: Processor[B]): Processor[A | B] { type ResultType = left.ResultType | right.ResultType } =
    P(_.fold(x => left process x map (Left(_)), x => right process x map (Right(_))))

  implicit def processPair[A, B](implicit first: Processor[A], second: Processor[B]): Processor[(A, B)] { type ResultType = (first.ResultType, second.ResultType) } = P {
    case (a, b) => first process a flatMap (a => second process b map (b => (a, b)))
  }

  implicit def optionProcessor[A](implicit processor: Processor[A]): Processor[Option[A]] { type ResultType = Option[processor.ResultType] } =
    P(_.map(processor process _ map (Option(_))).getOrElse(Result(None, empty)))

  implicit def seqProcessor[A](implicit processor: Processor[A]): Processor[Seq[A]] { type ResultType = Seq[processor.ResultType] } =
    P(_.map(processor.process).foldLeft(Result(Seq.empty[processor.ResultType], empty)) {
      (result, element) =>
        for {
          seq <- result
          e <- element
        } yield seq :+ e
    })

  implicit def nonEmptySeqProcessor[A](implicit processor: Processor[A]): Processor[NonEmptySeq[A]] { type ResultType = NonEmptySeq[processor.ResultType] } = P {
    _.map(processor.process).foldLeft(_.map(a => NonEmptySeq(a, Seq.empty))) {
      (result, element) =>
        for {
          seq <- result
          e <- element
        } yield seq :+ e
    }
  }
}
object SyntaxProcessor {

  // question: Should arguments have type parameters?


}
