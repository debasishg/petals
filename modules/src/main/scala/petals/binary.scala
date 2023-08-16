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
