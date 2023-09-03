package petals
package ch7

import ch6.Queue

object realTimeQueue:
  class RealTimeQueue[A] private (
      private val front: Stream[A],
      private val rear: List[A],
      private val schedule: Stream[A]
  ) extends Queue[A]:

    override def head: A = (front: @unchecked) match {
      case Stream.Empty => throw new NoSuchElementException("head on empty queue")
      case x #:: _      => x
    }

    override def tail: Queue[A] = (front: @unchecked) match {
      case Stream.Empty => throw new NoSuchElementException("tail on empty queue")
      case _ #:: xs     => exec(xs, rear, schedule)
    }

    override def snoc[B >: A](b: B): Queue[B] = exec(front, b :: rear, schedule)

    override def isEmpty: Boolean = front == Stream.Empty

    /** `exec` maintains the invariant that schedule.size = front.size - rear.size. Hence front.size = schedule.size +
      * rear.size. So size = front.size + rear.size = schedule.size + 2 * rear.size. (Exercise 7.2)
      */
    def size = schedule.size + 2 * rear.size

    private def rotate[B](f: Stream[B], r: List[B], a: Stream[B]): Stream[B] = ((f, r): @unchecked) match {
      case (Stream.Empty, y :: _) => y #:: a
      case (x #:: xs, y :: ys)    => x #:: rotate(xs, ys, y #:: a)
    }

    private def exec[B](f: Stream[B], r: List[B], a: Stream[B]): RealTimeQueue[B] = (a: @unchecked) match {
      case x #:: s => new RealTimeQueue[B](f, r, s)
      case Stream.Empty =>
        val g = rotate(f, r, Stream.Empty)
        new RealTimeQueue[B](g, r, g)
    }

  object RealTimeQueue:
    def empty[A]: RealTimeQueue[A] = new RealTimeQueue[A](Stream(), Nil, Stream())
