// P04 (*) Find the number of elements of a list.
//     Example:
//     scala> length(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 6

object P04 {
  def length[A](list: List[A]) = {
    def lenRec[A](curLen: Int, remainder: List[A]): Int = remainder match {
        case Nil => curLen
        case _ :: xs => lenRec(curLen + 1, xs)
    }  
    lenRec(0, list)
  }

  //this is not what I initially came up with myself but rather what I saw when comparing my solutions to the OG-author's
  def lengthFunc[A](list: List[A]) = list.foldLeft(0) {(current, _) => current + 1}
}
