package petals.ch9

object redundantZerolessBinarylist:
  enum Tree[+A]:
    case Leaf(a: A)                                            extends Tree[A]
    case Node(subtreeSize: Int, left: Tree[A], right: Tree[A]) extends Tree[A]
    case Empty                                                 extends Tree[Nothing]

    def size: Int = this match
      case Leaf(_) | Empty  => 1
      case Node(size, _, _) => size

  enum Digit[+A]:
    case One(tree: Tree[A])                                    extends Digit[A]
    case Two(tree1: Tree[A], tree2: Tree[A])                   extends Digit[A]
    case Three(tree1: Tree[A], tree2: Tree[A], tree3: Tree[A]) extends Digit[A]

  import Tree.*
  import Digit.*

  /** Constructs a new tree from 2 equal sized subtrees and automatically calculates the size of the new tree.
    */
  def link[A](t1: Tree[A], t2: Tree[A]): Tree[A] = Node(t1.size + t2.size, t1, t2)

  type BinaryList[+A] = Stream[Digit[A]]

  def consTree[A](tree: Tree[A], list: BinaryList[A]): BinaryList[A] = list match
    case Stream.Empty               => Stream(One(tree))
    case One(t1) #:: rest           => Two(tree, t1) #:: rest
    case Two(t1, t2) #:: rest       => Three(tree, t1, t2) #:: rest
    case Three(t1, t2, t3) #:: rest => Two(tree, t1) #:: consTree(link(t2, t3), rest)
    case _                          => throw new Exception("Invariant violated")

  /** When applied to a list whose first digit has rank r, `unconsTree` returns a pair containing a tree of rank r, and
    * the new list without that tree. `unconsTree` follows the rules of decrementing a binary number
    */
  def unConsTree[A](list: BinaryList[A]): (Tree[A], BinaryList[A]) = list match
    case Stream.Empty => throw new Exception("Index out of bounds")
    case One(t) #:: ts =>
      val (Node(_, t1, t2), rest) = unConsTree(ts): @unchecked
      (t, Two(t1, t2) #:: rest)
    case Two(t1, t2) #:: rest       => (t1, One(t2) #:: rest)
    case Three(t1, t2, t3) #:: rest => (t1, Two(t2, t3) #:: rest)
    case _                          => throw new Exception("Invariant violated")
