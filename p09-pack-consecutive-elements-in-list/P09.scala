// P09 (**) Pack consecutive duplicates of list elements into sublists.
//     If a list contains repeated elements they should be placed in separate
//     sublists.
//
//     Example:
//     scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

object P09 {
  def pack[A](list: List[A]) = {
    def packInner[A](rslt: List[List[A]], src: List[A]): List[List[A]] = src match {
      case x :: _   => src span (_ == x) match {
        case (packed, tail) => packInner(packed :: rslt, tail)
      } 
      case Nil      => rslt
    }
    packInner(Nil, list).reverse
  }
  
  def packFunc[A](list: List[A]) = list.foldRight[List[List[A]]] (Nil) {
    case (el, Nil)  => List(List(el))
    case (el, x :: xs) => if (x.head == el) (el :: x) :: xs else List(el) :: x :: xs
  }


  def packFunc2[A](list: List[A]) = list.foldRight[List[List[A]]] (Nil) {
    (el, rslt) => if (rslt.isEmpty || rslt.head.head != el) List(el) :: rslt else (el :: rslt.head) :: rslt.tail
  }

  val sample = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
}
