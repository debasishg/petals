package petals

import redundantZerolessBinarylist.*
import Digit.*
import Tree.*

// implementation of typeclass `RandomAccessList` for `RedundantZerolessBinaryList`
// Exercise 9.9
object RandomAccessListRedundantZerolessBinaryList:
  given RandomAccessList[BinaryList] with
    extension [A](fa: BinaryList[A])
      def isEmpty: Boolean = fa.isEmpty

      def cons(a: A): BinaryList[A] = consTree(Tree.Leaf(a), fa)

      def head: A = fa match
        case Stream.Empty               => throw new Exception("Index out of bounds")
        case One(Leaf(x)) #:: _         => x
        case Two(Leaf(x), _) #:: _      => x
        case Three(Leaf(x), _, _) #:: _ => x
        case _                          => throw new Exception("Invariant violated: the first digit needs to be a Leaf")

      def tail: BinaryList[A] =
        val (_, rest) = unConsTree(fa): @unchecked
        rest

      def lookup(i: Int): A = ???

      def update(i: Int)(a: A): BinaryList[A] = ???

      /** Using redundant zeroless binary, we ensure that the first digit is always non-zero, and `head` doesn't change
        * the list - hence `head` runs in O(1) time
        *
        * Allocate 1 debit for `Two` and 0 debits for `One` and `Three`.
        *
        * Say we have first k `Three`s, the `cons` changes k `Three`s to k `Two`s. Allocate 1 debit for each of these
        * steps. Now each of the `Two`s has 1 debit and if another `Two` is generated after the `Three`s, discharge 1
        * debit for it. This restores the invariant.
        *
        * Say we have first k `One`s, the `tail` changes k `One`s to k `Two`s. Allocate 1 debit for each of these steps.
        * Now each of the `Two`s has 1 debit and if another `Two` is generated after the `One`s, discharge 1 debit for
        * it. This restores the invariant.
        *
        * Hence cost of `cons` and `tail` is O(1)
        */
