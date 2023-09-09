package petals
package ch6

trait Queue[A]:
  def head: A
  def tail: Queue[A]
  def isEmpty: Boolean
  def snoc[B >: A](b: B): Queue[B]
