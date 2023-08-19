package petals

object binary:
  enum Digit:
    case Zero
    case One

    def inc(digits: List[Digit]): List[Digit] = digits match
      case Nil          => List(One)
      case Zero :: rest => One :: rest
      case One :: rest  => Zero :: inc(rest) // carry

    def dec(digits: List[Digit]): List[Digit] = digits match
      case Nil | One :: Nil => Nil
      case One :: rest      => Zero :: rest
      case Zero :: rest     => One :: dec(rest) // borrow

object zerolessBinary:
  enum Digit:
    case One
    case Two

    def inc(digits: List[Digit]): List[Digit] = digits match
      case Nil         => List(One)
      case One :: rest => Two :: rest
      case Two :: rest => One :: inc(rest) // carry

    def dec(digits: List[Digit]): List[Digit] = digits match
      case Nil | One :: Nil => Nil
      case Two :: rest      => One :: rest
      case One :: rest      => Two :: dec(rest) // borrow

    def add(ds1: List[Digit], ds2: List[Digit]): List[Digit] = (ds1, ds2) match
      case (Nil, ds)                => ds
      case (ds, Nil)                => ds
      case (One :: ds1, One :: ds2) => Two :: add(ds1, ds2)
      case (Two :: ds1, Two :: ds2) => Two :: inc(inc(add(ds1, ds2)))
      case (d1 :: ds1, d2 :: ds2)   => One :: inc(add(ds1, ds2))
