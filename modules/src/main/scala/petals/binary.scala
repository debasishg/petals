package petals

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
object binary:
  enum Binary:
    case Zero
    case One

    def inc(digits: List[Binary]): List[Binary] = digits match
      case Nil          => List(One)
      case Zero :: rest => One :: rest
      case One :: rest  => Zero :: inc(rest) // carry

    def dec(digits: List[Binary]): List[Binary] = digits match
      case Nil | One :: Nil => Nil
      case One :: rest      => Zero :: rest
      case Zero :: rest     => One :: dec(rest) // borrow

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

  type RList[A] = List[Digit[A]]

  /** Constructs a new tree from 2 equal sized subtrees and automatically calculates the size of the new tree.
    */
  def link[A](t1: Tree[A], t2: Tree[A]): Tree[A] = Tree.Node(t1.size + t2.size, t1, t2)

  /** To add a new element to the front of the list, we first convert the element into a leaf and insert the leaf into
    * the list of trees using the heler function `consTree` that follows the rules of incrementing a binary number.
    * `cons` does O(1) work per digit, and `consTree` does O(log n) work, so `cons` is O(log n) worst case.
    */
  def cons[A](a: A, list: RList[A]): RList[A] =
    def consTree[A](tree: Tree[A], list: RList[A]): RList[A] = list match
      case Nil                        => List(Digit.One(tree))
      case Digit.Zero :: rest         => Digit.One(tree) :: rest
      case Digit.One(subtree) :: rest => Digit.Zero :: consTree(link(tree, subtree), rest)

    consTree(Tree.Leaf(a), list)

  /** When applied to a list whose first digit has rank r, `unconsTree` returns a pair containing a tree of rank r, and
    * the new list without that tree. `unconsTree` follows the rules of decrementing a binary number
    */
  def unConsTree[A](list: RList[A]): (Tree[A], RList[A]) = list match
    case Nil                  => (Tree.Empty, Nil)
    case Digit.One(t) :: Nil  => (t, Nil)
    case Digit.One(t) :: rest => (t, Digit.Zero :: rest)
    case Digit.Zero :: rest =>
      val (Tree.Node(_, t1, t2), rest1) = unConsTree(rest): @unchecked
      (t1, Digit.One(t2) :: rest1)

  /** `head` and `tail` perform O(1) work per digit, and so run in O(log n) worst case time
    */
  def head[A](ts: RList[A]): A =
    val (Tree.Leaf(x), _) = unConsTree(ts): @unchecked
    x

  def tail[A](ts: RList[A]): RList[A] =
    val (_, rest) = unConsTree(ts): @unchecked
    rest
