package petals.ch7

object bankersQueue:
  class BankersQueue[A] private (
      private val fsize: Int,
      private val front: Stream[A],
      private val rsize: Int,
      private val rear: List[A]
  ) extends Queue[A]:
    val length: Int = fsize + rsize

    override def head: A = front match {
      case hd #:: _ => hd
      case _        => throw new NoSuchElementException("head on empty queue")
    }

    override def tail: BankersQueue[A] = front match {
      case _ #:: tail => check(new BankersQueue(fsize - 1, tail, rsize, rear))
      case _          => throw new NoSuchElementException("tail on empty queue")
    }

    private def check[B](q: BankersQueue[B]): BankersQueue[B] = {
      if (q.rsize <= q.fsize) q
      else new BankersQueue(q.fsize + q.rsize, q.front ++ q.rear.reverse, 0, Nil)
    }

    override def isEmpty: Boolean = length == 0

    override def snoc[B >: A](b: B): Queue[B] = check(new BankersQueue(fsize, front, rsize + 1, b :: rear))

  object BankersQueue:
    def empty[A]: BankersQueue[A] = new BankersQueue[A](0, Stream(), 0, Nil)
