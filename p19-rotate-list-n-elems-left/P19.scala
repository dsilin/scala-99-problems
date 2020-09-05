// P19 (**) Rotate a list N places to the left.
//     Examples:
//     scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
//
//     scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

object P19 {
  //this doesn't allow rotating multiple times if n is bigger than the length of the list
  def rotate[A](n: Int)(list: List[A]) = {
    if (n == 0 || list.isEmpty)     list
    else if (n > 0) (list splitAt n) match { case (xs, ys) => ys ::: xs }
    else            (list.reverse splitAt -n) match { case (xs, ys) => xs.reverse ::: ys.reverse }
  }

  //this does
  def rotateN[A](n: Int)(list: List[A]): List[A] = {
    val len = list.length
    //adjusting the shift so that it's not escaping the length of the list
    val nAdj = if (len == 0) 0 else n % len
    //taking care of the negative
    if (nAdj < 0) rotateN(nAdj + len)(list)
    else          (list splitAt nAdj) match { case (xs, ys) => ys ::: xs  }
  }
}
