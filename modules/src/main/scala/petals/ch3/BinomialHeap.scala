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

    override def merge(other: BinomialHeap[A]): BinomialHeap[A] =
      new BinomialHeap(merge(impl, other.impl))

    private def merge(one: List[Tree[A]], two: List[Tree[A]]): List[Tree[A]] = (one, two) match {
      case (Nil, _) => two
      case (_, Nil) => one
      case (ts1 @ (t1 :: ts11), ts2 @ (t2 :: ts22)) =>
        if (rank(t1) < rank(t2)) t1 :: merge(ts11, ts2)
        else if (rank(t2) < rank(t1)) t2 :: merge(ts1, ts22)
        else link(t1, t2) :: merge(ts11, ts22)
    }

    override def deleteMin = {
      val (Node(_, _, ts1), ts2) = removeMinTree(impl): @unchecked
      new BinomialHeap(ts1.reverse).merge(new BinomialHeap(ts2))
    }

    override def findMin: A = findMin(impl)

    private def findMin(trees: List[Tree[A]]): A = {
      val (t, _) = removeMinTree(trees)
      root(t)
    }

    /** Exercise 3.5: A direct implementation of `findMin` that does not use `removeMinTree`.
      */
    def findMinDirect: A = findMinDirect(impl)

    private def findMinDirect(trees: List[Tree[A]]): A = trees match {
      case Nil     => throw new Exception("Empty heap")
      case List(t) => root(t)
      case t :: ts =>
        val min = findMinDirect(ts)
        if (implicitly[Ordering[A]].lteq(root(t), min)) root(t)
        else min
    }

    override def isEmpty: Boolean = impl.isEmpty

    /** Worst case is inserting into a heap of size 2^k - 1, which requires k `link`s and O(k) = O(log n). Hence worst
      * case complexity of `insert` is O(log n)
      */
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
