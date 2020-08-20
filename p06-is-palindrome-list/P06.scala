// P06 (*) Find out whether a list is a palindrome.
//     Example:
//     scala> isPalindrome(List(1, 2, 3, 2, 1))
//     res0: Boolean = true

object P06 {

  //Yup, now I know that it's just list == list.reverse
  //Anyway, this is what I came up with initially instead
  //this probably means I'm bad with seeing an obvious solution
  //well, at list I got to apply some language features :D (and it's still O(n) (right?))
  def isPalindrome[A](list: List[A]) = {
    val size = list.size
    val splitIdx = if (size % 2 == 0) size / 2 else size / 2 + 1
    (list splitAt splitIdx) match {
      case (firstHalf, secondHalf) => (firstHalf zip secondHalf.reverse) forall {case (x, y) => x == y} 
    }
  }

  def genPalindrome(size: Int) = for (i <- (1 to size).toList) yield if (i <= size / 2) i else size - i + 1
}
