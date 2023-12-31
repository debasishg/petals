package petals.ch9

import zerolessBinarylist.*
import Digit.*
import Tree.*

// implementation of typeclass `RandomAccessList` for `ZerolessBinaryList`
// Exercise 9.5
object RandomAccessListZerolessBinaryList:
  given RandomAccessList[BinaryList] with
    extension [A](fa: BinaryList[A])
      def isEmpty: Boolean = fa.isEmpty

      /** To add a new element to the front of the list, we first convert the element into a leaf and insert the leaf
        * into the list of trees using the helper function `consTree` that follows the rules of incrementing a binary
        * number. `cons` does O(1) work per digit, and `consTree` does O(log n) work, so `cons` is O(log n) worst case.
        */
      def cons(a: A): BinaryList[A] = consTree(Tree.Leaf(a), fa)

      /** Using zeroless binary, we ensure that the first digit is always non-zero, hence `head` runs in O(1) worst case
        * time
        */
      def head: A = fa match
        case Nil                  => throw new Exception("Index out of bounds")
        case One(Leaf(x)) :: _    => x
        case Two(Leaf(x), _) :: _ => x
        case _                    => throw new Exception("Invariant violated: the first digit needs to be a Leaf")

      def tail: BinaryList[A] =
        val (_, rest) = unConsTree(fa): @unchecked
        rest

      /** `lookup` and `update` run in O(log i) worst case time, where i is the index of the element being accessed.
        * Assume lookup for position i is found in level k. Then we have: 1 + 2 + 4 + .. + 2^(k-1) < i, which implies
        * summing the GP series on the left 2^k - 1 < i, which implies 2^k < i + 1, which implies k < log(i + 1). Now
        * the size of the tree is 2^k - 1 and the looked up node is at depth k, so the total work done is O(log i). This
        * is an improvement over O(log n) for `lookup` and `update` with binary representation (Exercise 9.6)
        */
      def lookup(i: Int): A = fa match
        case Nil => throw new Exception("Index out of bounds")
        case One(t) :: rest =>
          if (i < t.size) lookupTree(i, t)
          else rest.lookup(i - t.size)
        case Two(t1, t2) :: rest =>
          if (i < t1.size) lookupTree(i, t1)
          else if (i < t1.size + t2.size) lookupTree(i - t1.size, t2)
          else rest.lookup(i - t1.size - t2.size)

      def update(i: Int)(a: A): BinaryList[A] = fa match
        case Nil => throw new Exception("Index out of bounds")
        case One(t) :: rest =>
          if (i < t.size) One(updateTree(i, a, t)) :: rest
          else One(t) :: rest.update(i - t.size)(a)
        case Two(t1, t2) :: rest =>
          if (i < t1.size) Two(updateTree(i, a, t1), t2) :: rest
          else if (i < t1.size + t2.size) Two(t1, updateTree(i - t1.size, a, t2)) :: rest
          else Two(t1, t2) :: rest.update(i - t1.size - t2.size)(a)
