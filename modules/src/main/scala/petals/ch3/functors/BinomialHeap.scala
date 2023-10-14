package petals.ch3
package functors

object binomialHeap:
  enum Tree[+A]:
    case Node(rank: Int, elem: A, children: List[Tree[A]]) extends Tree[A]
    case Empty                                             extends Tree[Nothing]

  import Tree.*
  import BinomialHeap.*

  class BinomialHeap[A: Ordering]() extends Heap[A]:
    type H = List[Tree[A]]

    override def empty = Nil

    override def merge(other: H)(h: H): H =
      _merge(other, h)

    private def _merge(one: H, two: H): H = (one, two) match {
      case (Nil, _) => two
      case (_, Nil) => one
      case (ts1 @ (t1 :: ts11), ts2 @ (t2 :: ts22)) =>
        if (rank(t1) < rank(t2)) t1 :: _merge(ts11, ts2)
        else if (rank(t2) < rank(t1)) t2 :: _merge(ts1, ts22)
        else link(t1, t2) :: _merge(ts11, ts22)
    }

    override def deleteMin(h: H): H = {
      val (Node(_, _, ts1), ts2) = removeMinTree(h): @unchecked
      merge(ts1.reverse)(ts2)
    }

    override def findMin(h: H): A = _findMin(h)

    private def _findMin(trees: H): A = {
      val (t, _) = removeMinTree(trees)
      root(t)
    }

    /** Exercise 3.5: A direct implementation of `findMin` that does not use `removeMinTree`. */
    def findMinDirect(h: H): A =
      _findMinDirect(h)

    private def _findMinDirect(trees: H): A =
      trees match {
        case Nil     => throw new Exception("Empty heap")
        case List(t) => root(t)
        case t :: ts =>
          val min = findMinDirect(ts)
          if (implicitly[Ordering[A]].lteq(root(t), min)) root(t) else min
      }

    override def isEmpty(h: H): Boolean = h match {
      case Nil => true
      case _   => false
    }

    /** Worst case is inserting into a heap of size 2^k - 1, which requires k `link`s and O(k) = O(log n). Hence worst
      * case complexity of `insert` is O(log n)
      */
    override def insert(a: A)(h: H) = insTree(Node(0, a, Nil), h)

  object BinomialHeap:

    private def insTree[A: Ordering](t: Tree[A], ts: List[Tree[A]]): List[Tree[A]] = ts match
      case Nil => List(t)
      case tsFirst :: tsRest =>
        if (rank(t) < rank(tsFirst)) t :: ts
        else insTree(link(t, tsFirst), tsRest)

    private def removeMinTree[A: Ordering](ts: List[Tree[A]]): (Tree[A], List[Tree[A]]) = ts match
      case Nil => throw new Exception("Empty heap")
      case t :: Nil =>
        (t, Nil)
      case t :: ts =>
        val (t1, ts1) = removeMinTree(ts)
        if (implicitly[Ordering[A]].lteq(root(t), root(t1))) (t, ts)
        else (t1, t :: ts1)

    private def link[A: Ordering](t1: Tree[A], t2: Tree[A]): Tree[A] = (t1, t2) match
      case (Node(r, x1, c1), Node(_, x2, c2)) =>
        if (implicitly[Ordering[A]].lteq(x1, x2)) Node(r + 1, x1, t2 :: c1)
        else Node(r + 1, x2, t1 :: c2)
      case _ => throw new Exception("Cannot link empty trees")

    private def rank[A](t: Tree[A]): Int = t match
      case Tree.Empty            => 0
      case Tree.Node(rank, _, _) => rank

    private def root[A](t: Tree[A]): A = t match
      case Tree.Empty            => throw new Exception("Empty tree")
      case Tree.Node(_, elem, _) => elem
