// P17 (*) Split a list into two parts.
//     The length of the first part is given.  Use a Tuple for your result.
//
//     Example:
//     scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

object P17 {
  def splitIntoTwo[A](firstLen: Int, list: List[A]) = {
    def splitRec[A](numToGo: Int, first: List[A], second: List[A]): (List[A], List[A]) = {
      if (0 >= numToGo || second.isEmpty) (first.reverse, second)
      else splitRec(numToGo - 1, second.head :: first, second.tail)
    }
    splitRec(firstLen, Nil, list)
  }

  //or :D
  def splitIntoTwo2[A](firstLen: Int, list: List[A]) = list splitAt firstLen
}
