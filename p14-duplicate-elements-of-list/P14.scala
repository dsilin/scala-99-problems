object P14 {
  def duplicate[A](list: List[A]) = list flatMap (el => List(el, el))

  def duplicate2[A](list: List[A]) = list.foldRight[List[A]] (Nil) ((el, rslt) => el :: el :: rslt)
}
