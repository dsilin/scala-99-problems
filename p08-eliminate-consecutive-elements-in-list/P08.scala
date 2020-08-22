// P08 (**) Eliminate consecutive duplicates of list elements.
//     If a list contains repeated elements they should be replaced with a
//     single copy of the element.  The order of the elements should not be
//     changed.
//
//     Example:
//     scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

object P08 {
  //didn't think about dropWhile
  def compress[A](list: List[A]) = {
    def compressInternal[A](result: List[A], src: List[A]): List[A] = (result, src) match {
      case (x :: _ , y :: ys) if (x == y) => compressInternal(result, ys) 
      case (_ , x :: xs)                  => compressInternal(x :: result, xs)
      case _                              => result
    } 
    compressInternal(Nil, list).reverse
  }

  //with dropWhile
  def compressWithDropWhile[A](list: List[A]) = {
    def compressInternal[A](result: List[A], src: List[A]): List[A] = src match {
      case x :: xs => compressInternal(x :: result, xs dropWhile (_ == x))
      case Nil => result
    }
    compressInternal(Nil, list).reverse
  }

  //using foldRight. this way we don't have to reverse eventually
  def compressFunc[A](list: List[A]) = list.foldRight(List[A]()) {
    (el, rslt) => if (rslt.isEmpty || rslt.head != el) el :: rslt else rslt
  }

  //sample input
  val sample = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
}
