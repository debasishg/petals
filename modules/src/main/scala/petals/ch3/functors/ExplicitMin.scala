package petals.ch3
package functors

// format: off
/** Scala representation of the OCaml functor from https://gist.github.com/debasishg/6c016d35f9b726fed5cf6b1a383eaeef.
  * Using the existential based implementation of modules that corresponds to functors and abstract data types. Also
  * inspired from https://github.com/yawaramin/scala-modules
  */
// format: on
object explicitMin:
  class ExplicitMin[Elem](private val heap: Heap[Elem])(using ord: Ordering[Elem]) extends Heap[Elem]:
    /** Representation which is abstract in `Heap`
      */
    sealed trait H
    private case object E                                    extends H
    private case class NE(elem: Elem, private val h: heap.H) extends H

    override val empty: H = E

    override def findMin(h: H): Elem = h match
      case E        => throw new Exception("Empty heap")
      case NE(e, _) => e

    override def deleteMin(h: H): H =
      h match
        case E => throw new Exception("Empty heap")
        case NE(_, h) =>
          val h1 = heap.deleteMin(h)
          if (heap.isEmpty(h1)) E
          else NE(heap.findMin(h1), h1)

    override def insert(a: Elem)(h: H): H =
      h match
        case E => NE(a, heap.insert(a)(heap.empty))
        case NE(e, h) =>
          val min = ord.min(a, e)
          if (min == a) NE(a, heap.insert(a)(h))
          else NE(e, heap.insert(a)(h))

    override def isEmpty(h: H): Boolean =
      h match
        case E        => true
        case NE(_, _) => false

    override def merge(other: H)(h: H): H =
      (h, other) match
        case (E, _) => other
        case (_, E) => h
        case (NE(e1, h1), NE(e2, h2)) =>
          val min = ord.min(e1, e2)
          if (min == e1) NE(e1, heap.merge(h1)(h2))
          else NE(e2, heap.merge(h1)(h2))

import binomialHeap._
import explicitMin._

implicit class Piper[A](val x: A) extends AnyVal:
  def |>[B](f: A => B) = f(x)

@main def main =
  val bin = new BinomialHeap[Int]
  val ex  = new ExplicitMin[Int](bin)

  // format: off
  ex.empty |> ex.insert(30) |> ex.insert(20) |> ex.insert(10) |> ex.findMin |> println 

  import ex._
  empty |> insert(30) |> insert(20) |> insert(10) |> findMin |> println 
  // format: on
