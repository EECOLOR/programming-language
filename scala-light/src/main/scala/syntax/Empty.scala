package syntax

class Empty[A](val value: A)
object Empty extends LowerPriorityEmpty {
  def empty[A](implicit empty: Empty[A]): A = empty.value

  implicit def forSeq[A]: Empty[Seq[A]] = new Empty(Seq.empty)
}
trait LowerPriorityEmpty {
  implicit def forOption[A]: Empty[Option[A]] = new Empty(None)
}
