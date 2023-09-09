package petals.ch3

object binomialHeap:
  enum Tree[+A]:
    case Node(rank: Int, elem: A, children: List[Tree[A]]) extends Tree[A]
    case Empty                                             extends Tree[Nothing]

  import Tree.*
  class BinomialHeap[A: Ordering](
      private val impl: List[Tree[A]]
  ) extends Heap[A, BinomialHeap[A]]:

    override def empty = new BinomialHeap[A](Nil)

    override def merge(other: BinomialHeap[A]): BinomialHeap[A] = (impl, other.impl) match {
      case (Nil, _) => other
      case (_, Nil) => this
      case (t1 :: ts1, t2 :: ts2) =>
        if (rank(t1) < rank(t2)) new BinomialHeap(t1 :: impl).merge(other)
        else if (rank(t2) < rank(t1)) merge(other)
        else new BinomialHeap(link(t1, t2) :: ts1).merge(new BinomialHeap(ts2))
    }

    override def deleteMin = {
      val (Node(_, _, ts1), ts2) = removeMinTree(impl): @unchecked
      new BinomialHeap[A](ts1.reverse ++ ts2)
    }

    override def findMin: A = {
      val (t, _) = removeMinTree(impl)
      root(t)
    }

    override def isEmpty: Boolean = impl.isEmpty

    override def insert(a: A) = new BinomialHeap[A](insTree(Node(0, a, Nil), impl))

    private def insTree(t: Tree[A], ts: List[Tree[A]]): List[Tree[A]] = ts match
      case Nil => List(t)
      case tp :: ts =>
        if (rank(t) < rank(tp)) t :: ts
        else insTree(link(t, tp), ts.tail)

    private def removeMinTree(ts: List[Tree[A]]): (Tree[A], List[Tree[A]]) = ts match
      case Nil => throw new Exception("Empty heap")
      case t :: Nil =>
        (t, Nil)
      case t :: ts =>
        val (t1, ts1) = removeMinTree(ts)
        if (implicitly[Ordering[A]].lteq(root(t), root(t1))) (t, ts)
        else (t1, t :: ts1)

    private def link(t1: Tree[A], t2: Tree[A]): Tree[A] = (t1, t2) match
      case (Node(r, x1, c1), Node(_, x2, c2)) =>
        if (implicitly[Ordering[A]].lteq(x1, x2)) Node(r + 1, x1, t2 :: c1)
        else Node(r + 1, x2, t1 :: c2)
      case _ => throw new Exception("Cannot link empty trees")

    private def rank(t: Tree[A]): Int = t match
      case Tree.Empty            => 0
      case Tree.Node(rank, _, _) => rank

    private def root(t: Tree[A]): A = t match
      case Tree.Empty            => throw new Exception("Empty tree")
      case Tree.Node(_, elem, _) => elem