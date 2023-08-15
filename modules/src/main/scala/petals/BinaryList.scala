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

  def link[A](t1: Tree[A], t2: Tree[A]): Tree[A] = Node(t1.size + t2.size, t1, t2)

  type BinaryList[+A] = List[Digit[A]]

  def consTree[A](tree: Tree[A], list: BinaryList[A]): BinaryList[A] = list match
    case Nil                  => List(One(tree))
    case Zero :: rest         => One(tree) :: rest
    case One(subtree) :: rest => Zero :: consTree(link(tree, subtree), rest)

  def unConsTree[A](list: BinaryList[A]): (Tree[A], BinaryList[A]) = list match
    case Nil            => (Empty, Nil)
    case One(t) :: Nil  => (t, Nil)
    case One(t) :: rest => (t, Zero :: rest)
    case Zero :: rest =>
      val (Node(_, t1, t2), rest1) = unConsTree(rest): @unchecked
      (t1, One(t2) :: rest1)
