package petals.ch3
package functors

trait Heap[Elem](using Ord: Ordering[Elem]):
  type H
  def empty: H
  def isEmpty(h: H): Boolean
  def insert(a: Elem)(h: H): H
  def merge(other: H)(h: H): H
  def findMin(h: H): Elem
  def deleteMin(h: H): H
