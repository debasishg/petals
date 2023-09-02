package petals.ch7

// typeclass
trait Queue[A]:
  def head: A
  def tail: Queue[A]
  def isEmpty: Boolean
  def snoc[B >: A](b: B): Queue[B]
