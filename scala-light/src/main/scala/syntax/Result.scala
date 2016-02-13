package syntax

import Empty.empty

case class Result[A, X](val value: A, val errors: Seq[X]) {

  def map[B](f: A => B): Result[B, X] = Result(f(value), errors)

  def flatMap[B](f: A => Result[B, X]): Result[B, X] = {
    val result = f(value)
    Result(result.value, errors ++ result.errors)
  }

  def withError(error: X): Result[A, X] =
    Result(value, errors :+ error)

  def withErrors[B >: A](newErrors: Seq[X]): Result[B, X] =
    Result(value, errors ++ newErrors)

    // for member extraction
  def withFilter(f: A => Boolean): Result[A, X] = this
}
object Result {
  def apply[A, X](value: A): Result[A, X] = Result(value, empty)
  def apply[A, X](value: A, error: X): Result[A, X] = Result(value, Seq(error))
  def apply[A, X](error: X)(implicit empty: Empty[A]): Result[A, X] = Result(empty.value, Seq(error))
}
