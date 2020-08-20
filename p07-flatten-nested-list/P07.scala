import scala.language.implicitConversions

object P07 {
  //this is my first try :D
  //looks ugly, for some reason I didn't think of pattern matching in map
  def flattenWithIfElse(list: List[Any]) = list flatMap f

  private def f(x: Any): List[Any] =
    if (x.isInstanceOf [List[Any]]) (x.asInstanceOf [List[Any]]) flatMap f
    else List(x)


  //this is how it should probably look like
  def flattenWithPatterns(list: List[Any]): List[Any] = list flatMap {
    case ls: List[_]  => flattenWithPatterns(ls)
    case el           => List(el)
  }

  //with implicits
  def flattenWithImplicit(list: List[Any]): List[Any] = list.flatten

  implicit def toIterableOnce(x: Any): List[Any] =  x match {
    case ls: List[_]  => ls.flatten
    case el           => List(el)
  }


  //imperative
  //This one doesn't scale as good as others since we have to do reverse in the end
  //and also we have to use list concatenation instead of simply assigning a new head.
  //This is going to be bad if nested lists are deep
  //
  //I just wanted to try not to use any collections std functions (well, besides reverse
  //but I could've imported my own implementation from one of the previous tasks)
  //and I was actually to lazy to implement smth more efficient with vars and while loops
  //like in collection's functions
  def flattenImper(list: List[Any]): List[Any] = {
    def flatten(result: List[Any], src: List[Any]): List[Any] = src match {
      case Nil      => result
      case x :: xs  => {
        val y = x match {
          case ls: List[_]  => flatten(Nil, ls)
          case el           => List(el)
        }
        flatten(y ::: result, xs)
      }
    }

    flatten(Nil, list).reverse
  }


  //some sample data to test this in repl
  val list = List(1, List(2,3,4), List(5, List(6,7)))
  val sampleLongList = List.fill(1_000_000)(list)
}
