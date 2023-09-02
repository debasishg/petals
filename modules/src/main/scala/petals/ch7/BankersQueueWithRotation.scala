package petals.ch7

object bankersQueueWithRotation:
  class BankersQueue[A] private (
      private val fsize: Int,
      private val front: Stream[A],
      private val rsize: Int,
      private val rear: Stream[A]
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
      else new BankersQueue(q.fsize + q.rsize, rotate(q.front, q.rear, Stream.Empty), 0, Stream.Empty)
    }

    override def isEmpty: Boolean = length == 0

    override def snoc[B >: A](b: B): Queue[B] = check(new BankersQueue(fsize, front, rsize + 1, b #:: rear))
  
    // format: off
    /** Exercise 7.1: Rotations occur when `rsize` >= `fsize` + 1. Assuming `fsize == m`, we have the length of the queue
      * as 2m for 1 call of `rotate`. Assuming the suspensions are not yet executed, for the next call, this becomes (2m + 2m) = 4m or 2 *
      * 2m. So the length of the queue will be at least:
      *
      * n >= 2m + 2 * 2m + 2^2 * 2m + ... + 2^(k - 1) * 2m
      *   = 2m * (1 + 2 + 2^2 + ... + 2^(k - 1))
      *   = 2m * (2^k - 1)
      * 
      * k <= log(n/2m + 1)
      * 
      * This is also the length of the rotate chain in the worst case. So the worst case time complexity of 
      * `head`, `tail` and `snoc` is O(log n) where n is the length of the queue.
      */
    // format: on
    private def rotate[B](f: Stream[B], r: Stream[B], a: Stream[B]): Stream[B] = (f, r) match {
      case (Stream.Empty, y #:: _) => y #:: a
      case (x #:: xs, y #:: ys)    => x #:: rotate(xs, ys, y #:: a)
      case _                       => throw new Exception("Invariant violated")
    }

  object BankersQueue:
    def empty[A]: BankersQueue[A] = new BankersQueue[A](0, Stream(), 0, Stream.Empty)
