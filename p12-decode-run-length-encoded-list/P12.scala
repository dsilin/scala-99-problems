object P12 {
  def decode[A](encoded: List[(Int, A)]) = encoded flatMap (fragment => List.fill(fragment._1)(fragment._2))
}
