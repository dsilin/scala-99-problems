// P01 (*) Find the last element of a list.
//     Example:
//     scala> last(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 8

object P01 {

  def last[A](list: List[A]): A = list match {
    case List(x) => x
    case x :: xs => last(xs)
    case Nil => throw new NoSuchElementException("Last element of an empty list")
  }


  //using options (contradicts the expected result type though)
  def lastOption[A](list: List[A]): Option[A] = list match {
      case Nil => None
      case List(x) => Some(x)
      case x :: xs => lastOption(xs)
  }
}
