// P13 (**) Run-length encoding of a list (direct solution).
//     Implement the so-called run-length encoding data compression method
//     directly.  I.e. don't use other methods you've written (like P09's
//     pack); do all the work directly.
//
//     Example:
//     scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

object P13 {
  //turns out I already implemented it in problem 10
  def encodeDirect[A](list: List[A]) = list.foldRight[List[(Int, A)]] (Nil) {
    case (el, Nil)                                    => List((1, el))
    case (el, (hNum, hVal) :: tail) if (el == hVal)   => (hNum + 1, hVal) :: tail
    case (el, rslt)                                   => (1, el) :: rslt
  }
}
