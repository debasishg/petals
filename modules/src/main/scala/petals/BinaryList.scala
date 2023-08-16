package petals

object binarylist:
  enum Tree[+A]:
    case Leaf(a: A)                                            extends Tree[A]
    case Node(subtreeSize: Int, left: Tree[A], right: Tree[A]) extends Tree[A]
    case Empty                                                 extends Tree[Nothing]

    def size: Int = this match
      case Leaf(_) | Empty  => 1
      case Node(size, _, _) => size

  enum Digit[+A]:
    case Zero               extends Digit[Nothing]
    case One(tree: Tree[A]) extends Digit[A]

  import Tree.*
  import Digit.*

  /** Constructs a new tree from 2 equal sized subtrees and automatically calculates the size of the new tree.
    */
  def link[A](t1: Tree[A], t2: Tree[A]): Tree[A] = Node(t1.size + t2.size, t1, t2)

  type BinaryList[+A] = List[Digit[A]]

  def consTree[A](tree: Tree[A], list: BinaryList[A]): BinaryList[A] = list match
    case Nil                  => List(One(tree))
    case Zero :: rest         => One(tree) :: rest
    case One(subtree) :: rest => Zero :: consTree(link(tree, subtree), rest)

  /** When applied to a list whose first digit has rank r, `unconsTree` returns a pair containing a tree of rank r, and
    * the new list without that tree. `unconsTree` follows the rules of decrementing a binary number
    */
  def unConsTree[A](list: BinaryList[A]): (Tree[A], BinaryList[A]) = list match
    case Nil            => (Empty, Nil)
    case One(t) :: Nil  => (t, Nil)
    case One(t) :: rest => (t, Zero :: rest)
    case Zero :: rest =>
      val (Node(_, t1, t2), rest1) = unConsTree(rest): @unchecked
      (t1, One(t2) :: rest1)

  def lookupTree[A](i: Int, t: Tree[A]): A = t match
    case Empty             => throw new Exception("Index out of bounds")
    case Leaf(x) if i == 0 => x
    case Leaf(_)           => throw new Exception("Index out of bounds")
    case Node(w, l, r) =>
      if (i < w / 2) lookupTree(i, l)
      else lookupTree(i - w / 2, r)

  def updateTree[A](i: Int, by: A, t: Tree[A]): Tree[A] = t match
    case Empty             => throw new Exception("Index out of bounds")
    case Leaf(x) if i == 0 => Leaf(by)
    case Leaf(_)           => throw new Exception("Index out of bounds")
    case Node(w, l, r) =>
      if (i < w / 2) Node(w, updateTree(i, by, l), r)
      else Node(w, l, updateTree(i - w / 2, by, r))
