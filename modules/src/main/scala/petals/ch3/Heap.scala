package petals.ch3

// based on: https://typelevel.org/blog/2016/11/17/heaps.html
trait Heap[Elem, H <: Heap[Elem, H]](using Ord: Ordering[Elem]):
  def empty: H
  def isEmpty: Boolean
  def insert(a: Elem): H
  def merge(other: H): H
  def findMin: Elem
  def deleteMin: H
