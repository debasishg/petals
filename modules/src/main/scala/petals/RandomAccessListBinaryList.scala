package petals

import binarylist.*
object RandomAccessListBinaryList:
  given RandomAccessList[BinaryList] with
    extension [A](fa: BinaryList[A])
      def isEmpty: Boolean          = fa.isEmpty
      def cons(a: A): BinaryList[A] = consTree(Tree.Leaf(a), fa)
      def head: A =
        val (Tree.Leaf(x), _) = unConsTree(fa): @unchecked
        x
      def tail: BinaryList[A] =
        val (_, rest) = unConsTree(fa): @unchecked
        rest
      def lookup(i: Int): A                   = ???
      def update(i: Int)(a: A): BinaryList[A] = ???
