package petals.ch9

// format: off
/** Implementation of random access lists using a binary numerical representation. A binary random access list of size
  * `n` contains a tree for each one in the binary representation of `n`.
  *
  * The rank of each tree corresponds to the rank of the corresponding digit. If the ith bit of `n` is 1, then the
  * random access list contains a tree of size 2^i. The least significant bit is at the head of the list.
  *
  * Trees are stored in increasing order of size, and the order of elements is left to right, both within and between
  * trees. Thus, the head of the random access list is the leftmost leaf of the smallest tree. The maximum number of
  * trees in a list of size n is floor(log(n + 1)) and the maximum depth of any tree is floor(log n).
  *
  * Here's an example of a binary random access list of size 7, corresponding to binary number 111:
  *
  *
  *                                        /  \
  *                                       /    \
  *                                      /      \
  *                         / \        / \     / \
  *               ( o  ,   o   o  ,   o   o   o   o )
  *                 ^        ^              ^
  *                 |        |              |
  *     (Rank 0 - Leaf)  (Rank 1 tree)  (Rank 2 tree)
  *                      (2^1 leaves)   (2^2 leaves)
  *
  * Now if we want to add an element to the front of the list, the size becomes 8, and we get the binary number 1000.
  * Here the addition of the element is isomorphic to incrementing the binary number. 
  * 
  *                                                 o
  *                                                 |
  *                                         + ----- + ----- +
  *                                         |               | 
  *                                     + - o - +       + - o - +
  *                                     |       |       |       |
  *                                    / \     / \     / \     / \
  *          ( Zero , Zero ,  Zero ,  o   o   o   o   o   o   o   o )
  *                                                 ^
  *                                                 |
  *                                            (Rank 3 tree)
  *                                            (2^3 leaves)
  *
  * Reference: Purely Functional Data Structures by Chris Okasaki (Chapter 9 - Numerical Representations)
  */
  // format: on

import binarylist.*

// implementation of typeclass `RandomAccessList` for `BinaryList`
object RandomAccessListBinaryList:
  given RandomAccessList[BinaryList] with
    extension [A](fa: BinaryList[A])
      def isEmpty: Boolean = fa.isEmpty

      /** To add a new element to the front of the list, we first convert the element into a leaf and insert the leaf
        * into the list of trees using the helper function `consTree` that follows the rules of incrementing a binary
        * number. `cons` does O(1) work per digit, and `consTree` does O(log n) work, so `cons` is O(log n) worst case.
        */
      def cons(a: A): BinaryList[A] = consTree(Tree.Leaf(a), fa)

      /** `head` and `tail` perform O(1) work per digit, and so run in O(log n) worst case time
        */
      def head: A =
        val (Tree.Leaf(x), _) = unConsTree(fa): @unchecked
        x

      def tail: BinaryList[A] =
        val (_, rest) = unConsTree(fa): @unchecked
        rest

      /** `lookup` and `update` do not have analogous arithmetic operations. Rather they take advantage of the
        * organization of binary random access lists as logarithmic-length lists of logarithmic-depth trees. Looking up
        * of an element is a 2 stage process. We first search the list for the correct tree, and then search the tree
        * for the correct element. The helper function `lookupTree` uses the `size` field in each node to determine
        * whether the ith element is in the left or the right subtree.
        */
      def lookup(i: Int): A = fa match
        case Nil => throw new Exception("Index out of bounds")
        case Digit.Zero :: rest =>
          rest.lookup(i)
        case Digit.One(t) :: rest =>
          if (i < t.size) lookupTree(i, t)
          else rest.lookup(i - t.size)

      def update(i: Int)(a: A): BinaryList[A] = fa match
        case Nil => throw new Exception("Index out of bounds")
        case Digit.Zero :: rest =>
          Digit.Zero :: rest.update(i)(a)
        case Digit.One(t) :: rest =>
          if (i < t.size) Digit.One(updateTree(i, a, t)) :: rest
          else Digit.One(t) :: rest.update(i - t.size)(a)

object App {
  import binarylist.*
  import Digit.*
  import Tree.*
  import RandomAccessListBinaryList.{ *, given }

  @main def main() =
    val b: BinaryList[Int] =
      List(Zero, One(Leaf(1)), Zero, One(Node(2, Leaf(2), Leaf(3))), Zero, One(Leaf(4)), One(Leaf(5)))
    println(b.lookup(0)) // 1
    println(b.lookup(1)) // 1
    println(b.lookup(3)) // 1

}
