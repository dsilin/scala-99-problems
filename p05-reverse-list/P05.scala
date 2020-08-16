// P05 (*) Reverse a list.
//     Example:
//     scala> reverse(List(1, 1, 2, 3, 5, 8))
//     res0: List[Int] = List(8, 5, 3, 2, 1, 1)

object P05 {
  def reverse[A](list: List[A]) = {
    def revRec[A](rev: List[A], og: List[A]): List[A] = og match {
      case Nil => rev
      case x :: xs => revRec(x :: rev, xs)
    }
    revRec(Nil, list)
  }

  def reverseFunc[A](list: List[A]) = list.foldLeft[List[A]](Nil) {(result, x) => x :: result}
}
