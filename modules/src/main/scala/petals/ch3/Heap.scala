package petals.ch3

// based on: https://typelevel.org/blog/2016/11/17/heaps.html
trait Heap[Elem, H <: Heap[Elem, H]](using Ord: Ordering[Elem]):
  def empty: H
  def isEmpty: Boolean
  def insert(a: Elem): H
  def merge(other: H): H
  def findMin: Elem
  def deleteMin: H

trait MHeap[Elem](using Ord: Ordering[Elem]):
  type H
  def empty: H
  def isEmpty(h: H): Boolean
  def insert(a: Elem)(h: H): H
  def merge(other: H)(h: H): H
  def findMin(h: H): Elem
  def deleteMin(h: H): H
