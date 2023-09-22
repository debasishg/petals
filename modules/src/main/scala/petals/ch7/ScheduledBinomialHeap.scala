package petals.ch7

import _root_.petals.ch3.Heap

object scheduledBinomialHeap:
  enum Tree[+A]:
    case Node(elem: A, children: List[Tree[A]]) extends Tree[A]
    case Empty                                  extends Tree[Nothing]

  enum Digit[+A]:
    case Zero               extends Digit[Nothing]
    case One(tree: Tree[A]) extends Digit[A]

  type Schedule[A] = List[Stream[Digit[A]]]
  type Repr[A]     = (Stream[Digit[A]], Schedule[A])

  import Tree.*
  import Digit.*

  class ScheduledBinomialHeap[A: Ordering](
      private val impl: Repr[A]
  ) extends Heap[A, ScheduledBinomialHeap[A]]:

    override def insert(a: A): ScheduledBinomialHeap[A] =
      val (ds, sched) = impl
      val ds1         = insTree(Node(a, Nil), ds)
      new ScheduledBinomialHeap[A]((ds1, exec(exec(ds1 :: sched))))

    override def deleteMin: ScheduledBinomialHeap[A] =
      val (ds, _)           = impl
      val (Node(_, c), ds1) = removeMinTree(ds): @unchecked
      val ds2               = mrgWithList(c.reverse, ds1)
      new ScheduledBinomialHeap[A]((ds2.force, Nil))

    override def merge(other: ScheduledBinomialHeap[A]): ScheduledBinomialHeap[A] =
      val ((ds1, _), (ds2, _)) = (impl, other.impl)
      val ds                   = mrg(ds1, ds2)
      new ScheduledBinomialHeap[A]((ds.force, Nil))

    override def isEmpty: Boolean = impl match
      case (Stream.Empty, _) => true
      case _                 => false

    override def findMin: A =
      val (ds, _)         = impl
      val (Node(x, _), _) = removeMinTree(ds): @unchecked
      x

    override def empty = new ScheduledBinomialHeap[A](Stream.Empty, Nil)

    private def insTree(t: Tree[A], ts: Stream[Digit[A]]): Stream[Digit[A]] = ts match
      case Stream.Empty   => Stream(One(t))
      case Zero #:: ds    => One(t) #:: ds
      case One(t1) #:: ds => Zero #:: insTree(link(t, t1), ds)
      case _              => ???

    private def link(t1: Tree[A], t2: Tree[A]): Tree[A] = (t1, t2) match
      case (Node(x1, c1), Node(x2, c2)) =>
        if (implicitly[Ordering[A]].lteq(x1, x2)) Node(x1, t2 :: c1) else Node(x2, t1 :: c2)
      case _ => throw new Exception("Cannot link empty trees")

    private def mrg(a: Stream[Digit[A]], b: Stream[Digit[A]]): Stream[Digit[A]] = (a, b) match
      case (ds1, Stream.Empty)                => ds1
      case (Stream.Empty, ds2)                => ds2
      case (Zero #:: ds1, d #:: ds2)          => d #:: mrg(ds1, ds2)
      case (d #:: ds1, Zero #:: ds2)          => d #:: mrg(ds1, ds2)
      case (One(t1) #:: ds1, One(t2) #:: ds2) => Zero #:: insTree(link(t1, t2), mrg(ds1, ds2))
      case _                                  => ???

    /** Exercise 7.4: Write an efficient, specialized version of `mrg`, called `mrgWithList`, so that `deleteMin` can
      * call `mrgWithList(rev c, ds')` instead of `mrg (listToStream(map ONE (rev c)), ds')`
      */
    def mrgWithList(a: List[Tree[A]], b: Stream[Digit[A]]): Stream[Digit[A]] = (a, b) match
      case (Nil, ds2)                   => ds2
      case (ds1, Stream.Empty)          => ds1.map(One[A]).toStream
      case (d :: ds1, Zero #:: ds2)     => One(d) #:: mrgWithList(ds1, ds2)
      case (t1 :: ds1, One(t2) #:: ds2) => Zero #:: insTree(link(t1, t2), mrgWithList(ds1, ds2))
      case _                            => ???

    private def exec(schedule: Schedule[A]): Schedule[A] = schedule match
      case Nil                     => Nil
      case (Zero #:: job) :: sched => job :: sched
      case _ :: sched              => sched

    private def removeMinTree(ds: Stream[Digit[A]]): (Tree[A], Stream[Digit[A]]) = ds match
      case Stream.Empty              => throw new Exception("removeMinTree on empty heap")
      case (One(t) #:: Stream.Empty) => (t, Stream.Empty)
      case (Zero #:: dss) =>
        val (t1, ds1) = removeMinTree(dss)
        (t1, Zero #:: ds1)
      case (One(t @ Node(x, _)) #:: dss) =>
        removeMinTree(dss) match
          case (t1 @ Node(x1, _), ds1) =>
            if (implicitly[Ordering[A]].lteq(x, x1)) (t, Zero #:: dss)
            else (t1, One(t) #:: ds1)
          case (Empty, _) => throw new Exception("Empty tree found in heap")
      case _ => ???
