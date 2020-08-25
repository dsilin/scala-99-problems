// P16 (**) Drop every Nth element from a list.
//     Example:
//     scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)


object P16 {
  def drop[A](n: Int)(list: List[A]) = {
    def dropRec[A](i: Int, src: List[A], rslt: List[A]): List[A] = (i, src) match {
      case (`n`, _ :: xs) => dropRec(1, xs, rslt)
      case (_, x :: xs)   => dropRec(i + 1, xs, x :: rslt)
      case _              => rslt
    }
    dropRec(1, list, Nil).reverse
  }

  //forgot zipWithIndex method existed
  def dropFunc[A](n: Int)(list: List[A]) = list.zipWithIndex filter (pair => (pair._2 + 1) % n != 0) map (_._1)
}
