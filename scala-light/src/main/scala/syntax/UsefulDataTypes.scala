package syntax

object UsefulDataTypes {
  type |[+A, +B] = Either[A, B]
  case class NonEmptySeq[A](head: A, tail: Seq[A]) {
    def toSeq: Seq[A] =
      head +: tail

    def map[B](f: A => B): NonEmptySeq[B] =
      NonEmptySeq(f(head), tail map f)

    def foldLeft[B](first: A => B)(rest: (B, A) => B): B =
      tail.foldLeft(first(head))(rest)

    def foldRight[B](first: A => B)(rest: (A, B) => B): B =
      tail.foldRight(first(head))(rest)

    def :+ (a: A): NonEmptySeq[A] =
      NonEmptySeq(head, tail :+ a)
  }

  object NonEmptySeq {
    object FromSeq {
      def unapply[A](x: Seq[A]): Option[NonEmptySeq[A]] =
        x.headOption.map(NonEmptySeq(_, x.tail))
    }
  }
}
