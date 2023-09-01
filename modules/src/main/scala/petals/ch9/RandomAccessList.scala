package petals.ch9

// typeclass
trait RandomAccessList[F[+_]]:
  extension [A](fa: F[A])
    def isEmpty: Boolean
    def cons(a: A): F[A]
    def head: A
    def tail: F[A]
    def lookup(i: Int): A
    def update(i: Int)(a: A): F[A]
