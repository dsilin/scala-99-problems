object P20 {
  def removeAt[A](pos: Int, list: List[A]): (List[A], A) = {
    if (pos < 0) throw new NoSuchElementException
    list drop pos match {
      case x :: xs  => ((list take pos) ::: xs, x)
      case _        => throw new NoSuchElementException
    }  
  }

  def removeAtRec[A](pos: Int, list: List[A]): (List[A], A) = {
    def removeRec[A](n: Int, src: List[A], init: List[A]): (List[A], A) = src match {
      case x :: xs  => if (n == 0) (init.reverse ::: xs, x) else removeRec(n - 1, xs, x :: init)
      case Nil      => throw new NoSuchElementException
    }
    if (pos < 0) throw new NoSuchElementException else removeRec(pos, list, Nil)
  }
}
