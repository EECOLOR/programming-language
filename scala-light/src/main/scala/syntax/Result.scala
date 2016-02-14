package syntax

import Empty.empty

class Result[A, X](val value: A, val errors: Seq[X]) {

  def map[B](f: A => B): Result[B, X] = new Result(f(value), errors)

  def flatMap[B](f: A => Result[B, X]): Result[B, X] = {
    val result = f(value)
    new Result(result.value, errors ++ result.errors)
  }

  def withError(error: X): Result[A, X] =
    new Result(value, errors :+ error)

  def withErrors[B >: A](newErrors: Seq[X]): Result[B, X] =
    new Result(value, errors ++ newErrors)

    // for member extraction
  def withFilter(f: A => Boolean): Result[A, X] = this
}
