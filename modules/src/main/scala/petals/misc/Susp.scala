package petals
package misc

object Susp {
  def apply[T](x: => T): Susp[T]                              = new Susp(x)
  def unapply[T](x: Susp[T]): Option[T]                       = Some(x())
  def lift2[T](f: (T, T) => T): (Susp[T], Susp[T]) => Susp[T] = (a, b) => Susp(f(a(), b()))
}

class Susp[T](x: => T) {
  lazy val forced: T    = x
  def apply(): T        = forced
  override def toString = "Susp(...)"
}
