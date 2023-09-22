package petals
package ch6

import ch3.Heap
import misc.Susp

object lazyBinomialHeap:
  enum Tree[+A]:
    case Node(rank: Int, elem: A, children: List[Tree[A]]) extends Tree[A]
    case Empty                                             extends Tree[Nothing]

  import Tree.*
  import LazyBinomialHeap.*

  class LazyBinomialHeap[A: Ordering](
      private val impl: Susp[List[Tree[A]]]
  ) extends Heap[A, LazyBinomialHeap[A]]:

    override def empty = new LazyBinomialHeap[A](Susp(Nil))

    override def merge(other: LazyBinomialHeap[A]): LazyBinomialHeap[A] =
      new LazyBinomialHeap(Susp.lift2(mergeTrees)(impl, other.impl))

    override def deleteMin = {
      val (Node(_, _, ts1), ts2) = removeMinTree(impl()): @unchecked
      new LazyBinomialHeap(Susp.lift2(mergeTrees)(Susp(ts1.reverse), Susp(ts2)))
    }

    override def findMin: A = findMinTrees(impl())

    override def isEmpty: Boolean = impl match {
      case Susp(Nil) => true
      case _         => false
    }

    /** O(1) amortized time bound even when the heaps are used peristently. For `BinomialHeap`, we had O(1) amortized
      * for ephemeral use, but O(log n) with persistence
      */
    override def insert(a: A) = new LazyBinomialHeap[A](Susp(insTree(Node(0, a, Nil), impl())))

    private def mergeTrees(one: List[Tree[A]], two: List[Tree[A]]): List[Tree[A]] = (one, two) match {
      case (Nil, _) => two
      case (_, Nil) => one
      case (ts1 @ (t1 :: ts11), ts2 @ (t2 :: ts22)) =>
        if (rank(t1) < rank(t2)) t1 :: mergeTrees(ts11, ts2)
        else if (rank(t2) < rank(t1)) t2 :: mergeTrees(ts1, ts22)
        else link(t1, t2) :: mergeTrees(ts11, ts22)
    }

  object LazyBinomialHeap:

    private def root[A](t: Tree[A]): A = t match
      case Tree.Empty            => throw new Exception("Empty tree")
      case Tree.Node(_, elem, _) => elem

    private def rank[A](t: Tree[A]): Int = t match
      case Tree.Empty            => 0
      case Tree.Node(rank, _, _) => rank

    private def link[A: Ordering](t1: Tree[A], t2: Tree[A]): Tree[A] = (t1, t2) match
      case (Node(r, x1, c1), Node(_, x2, c2)) =>
        if (implicitly[Ordering[A]].lteq(x1, x2)) Node(r + 1, x1, t2 :: c1)
        else Node(r + 1, x2, t1 :: c2)
      case _ => throw new Exception("Cannot link empty trees")

    private def removeMinTree[A: Ordering](ts: List[Tree[A]]): (Tree[A], List[Tree[A]]) = ts match
      case Nil => throw new Exception("Empty heap")
      case t :: Nil =>
        (t, Nil)
      case t :: ts =>
        val (t1, ts1) = removeMinTree(ts)
        if (implicitly[Ordering[A]].lteq(root(t), root(t1))) (t, ts)
        else (t1, t :: ts1)

    private def insTree[A: Ordering](t: Tree[A], ts: List[Tree[A]]): List[Tree[A]] = ts match
      case Nil => List(t)
      case tp :: ts =>
        if (rank(t) < rank(tp)) t :: ts
        else insTree(link(t, tp), ts.tail)

    private def findMinTrees[A: Ordering](trees: List[Tree[A]]): A = {
      val (t, _) = removeMinTree(trees)
      root(t)
    }
