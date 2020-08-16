// P02 (*) Find the last but one element of a list.
//     Example:
//     scala> penultimate(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 5


object P02 {
  def penultimate[A](list: List[A]): A = list match {
    case x :: _ :: Nil => x
    case x :: xs => penultimate(xs)
    case _ => throw new NoSuchElementException
  }
}
