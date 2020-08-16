// P03 (*) Find the Kth element of a list.
//     By convention, the first element in the list is element 0.
//
//     Example:
//     scala> nth(2, List(1, 1, 2, 3, 5, 8))
//     res0: Int = 2

object P03 {
  def nth[A](n: Int, list: List[A]): A = {
    if (n < 0) throw new IllegalArgumentException("n must be >= 0")
    list match {
      case x :: xs => if (n == 0) x else nth(n - 1, xs)
      case Nil => throw new NoSuchElementException("The list is exhausted before the nth element is reached")
    }
  }
}
