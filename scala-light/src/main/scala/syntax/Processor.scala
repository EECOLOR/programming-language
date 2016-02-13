package syntax

import Empty.empty
import UsefulDataTypes.{
  |,
  NonEmptySeq
}
import scala.language.implicitConversions

trait Processor[-A] {
  type ResultType
  type ErrorType
  val process: A => Result[ResultType, ErrorType]
}
object Processor {

  import scala.language.implicitConversions

  implicit def injectLeft [A, B](x: A): A | B = Left(x)
  implicit def injectRight[A, B](x: B): A | B = Right(x)

  implicit def merge[A](x: A | A): A = x.merge

  def process[A](ast: A)(implicit processor: Processor[A]): Result[processor.ResultType, processor.ErrorType] =
    processor process ast

  def context[A]: Context[A] = new Context[A]

  sealed class Context[X] {
    type ->[A, B] = Processor[A] {
      type ResultType = B
      type ErrorType = X
    }

    def P[A, B](f: A => Result[B, X]): (A -> B) =
      new Processor[A] {
        type ResultType = B
        type ErrorType = X
        val process = f
      }

    implicit def coproductProcessor[A, B](implicit left: Processor[A] { type ErrorType = X }, right: Processor[B] { type ErrorType = X }):
      (A | B) -> (left.ResultType | right.ResultType) = P {
        _.fold(left process _ map injectLeft, right process _ map injectRight)
      }

    implicit def productProcessor[A, B](implicit first: Processor[A] { type ErrorType = X }, second: Processor[B] { type ErrorType = X }):
      (A, B) -> (first.ResultType, second.ResultType) = P {
        case (a, b) => for {
          newA <- first process a
          newB <- second process b
        } yield (newA, newB)
      }

    implicit def optionProcessor[A](implicit processor: Processor[A] { type ErrorType = X }):
      Option[A] -> Option[processor.ResultType] = P {
        _.map(processor process _ map (Option(_))).getOrElse(Result(None, empty))
      }

    implicit def seqProcessor[A](implicit processor: Processor[A] { type ErrorType = X }):
      Seq[A] -> Seq[processor.ResultType] = P {
        _.map(processor.process).foldLeft(Result(Seq.empty[processor.ResultType], Seq.empty[X])) {
          (result, element) =>
            for {
              seq <- result
              e <- element
            } yield seq :+ e
        }
      }

    implicit def nonEmptySeqProcessor[A](implicit processor: Processor[A] { type ErrorType = X }):
      NonEmptySeq[A] -> NonEmptySeq[processor.ResultType] = P {
        _.map(processor.process).foldLeft(_.map(NonEmptySeq(_, empty))) {
          (result, element) =>
            for {
              seq <- result
              e <- element
            } yield seq :+ e
        }
      }
  }
}
