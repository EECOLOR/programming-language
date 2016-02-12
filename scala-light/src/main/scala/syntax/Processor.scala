package syntax

import Empty.empty
import UsefulDataTypes.{
  |,
  NonEmptySeq
}
import scala.language.implicitConversions

trait Processor[-A] {
  type ResultType
  val process: A => Result[ResultType]
}
object Processor extends DefaultProcessors {

  import scala.language.implicitConversions

  implicit def injectLeft [A, B](x: A): A | B = Left(x)
  implicit def injectRight[A, B](x: B): A | B = Right(x)

  implicit def merge[A](x: A | A): A = x.merge

  def process[A](ast: A)(implicit processor: Processor[A]): Result[processor.ResultType] =
    processor process ast

  type ->[A, B] = Processor[A] { type ResultType = B }
  def P[A, B](f: A => Result[B]): A -> B =
    new Processor[A] {
      type ResultType = B
      val process = f
    }
}

trait DefaultProcessors {

  import Processor._

  implicit def coproductProcessor[A, B](implicit left: Processor[A], right: Processor[B]):
    (A | B) -> (left.ResultType | right.ResultType) = P {
      _.fold(left process _ map injectLeft, right process _ map injectRight)
    }

  implicit def productProcessor[A, B](implicit first: Processor[A], second: Processor[B]):
    (A, B) -> (first.ResultType, second.ResultType) = P {
      case (a, b) => for {
        newA <- first process a
        newB <- second process b
      } yield (newA, newB)
    }

  implicit def optionProcessor[A](implicit processor: Processor[A]):
    Option[A] -> Option[processor.ResultType] = P {
      _.map(processor process _ map (Option(_))).getOrElse(Result(None, empty))
    }

  implicit def seqProcessor[A](implicit processor: Processor[A]):
    Seq[A] -> Seq[processor.ResultType] = P {
      _.map(processor.process).foldLeft(Result(Seq.empty[processor.ResultType], empty)) {
        (result, element) =>
          for {
            seq <- result
            e <- element
          } yield seq :+ e
      }
    }

  implicit def nonEmptySeqProcessor[A](implicit processor: Processor[A]):
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
