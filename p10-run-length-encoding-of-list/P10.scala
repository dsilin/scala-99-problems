// P10 (*) Run-length encoding of a list.
//     Use the result of problem P09 to implement the so-called run-length
//     encoding data compression method.  Consecutive duplicates of elements are
//     encoded as tuples (N, E) where N is the number of duplicates of the
//     element E.
//
//     Example:
//     scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

// to compile: 
// scalac ../p09-pack-consecutive-elements-in-list/P09.scala P10.scala

object P10 {
  //probably not as efficient as it could've been with some mutable state
  def runLength[A](list: List[A]) = (P09 packFunc2 list) map (pack => (pack.length, pack.head))

  //I didn't benchmark but this should run faster since it traverces the collection only once
  def runLength2[A](list: List[A]) = list.foldRight[List[(Int, A)]] (Nil) {
    case (el, Nil)                                    => List((1, el))
    case (el, (hNum, hVal) :: tail) if (el == hVal)   => (hNum + 1, hVal) :: tail
    case (el, rslt)                                   => (1, el) :: rslt
  }
}
