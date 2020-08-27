// P18 (**) Extract a slice from a list.
//     Given two indices, I and K, the slice is the list containing the elements
//     from and including the Ith element up to but not including the Kth
//     element of the original list.  Start counting the elements with 0.
//
//     Example:
//     scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: List[Symbol] = List('d, 'e, 'f, 'g)

object P18 {
  def slice[A](from: Int, until: Int, list: List[A]) = {
    def sliceRec[A](f: Int, u: Int, src: List[A], rslt: List[A]): List[A] = {
      if (0 >= u || src.isEmpty)  rslt
      else if (0 >= f)            sliceRec(f, u - 1, src.tail, src.head :: rslt)
      else                        sliceRec(f - 1, u - 1, src.tail, rslt)
    }
    sliceRec(from, until, list, Nil).reverse
  }

  def slice2[A](from: Int, until: Int, list: List[A]) = list slice (from, until)

  def slice3[A](from: Int, until: Int, list: List[A]) = (list drop from) take (until - (from max 0))
}
