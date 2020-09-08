// P21 (*) Insert an element at a given position into a list.
//     Example:
//     scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
//     res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)

object P21 {
  //didn't know what to do if the position is out of range
  //so here I'm allowing to insert the element to the end only if the position is: indexOf(list.last) + 1
  def insert[A](elem: A, pos: Int, list: List[A]) = {
    if (pos < 0 || pos > list.size) throw new IllegalArgumentException
    if (pos == 0) elem :: list
    else (list take pos) ::: elem :: (list drop pos)    
  }


  //this one allows prepending/appending elements no matter how smaller/bigger the position 
  //is relative to start/end index  
  def insertAt[A](elem: A, pos: Int, list: List[A]) = list splitAt pos match {
    case (beforePos, startingFromPos) => beforePos ::: elem :: startingFromPos 
  }
}
