package syntax

import Empty.empty

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
}
