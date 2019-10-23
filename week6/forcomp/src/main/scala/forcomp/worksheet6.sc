object worksheet6 {

  type Occurrences = List[(Char, Int)]
  type Word = String
  val dictionary: List[Word] = List("I", "love", "you", "vole")


  val x = "apple".toCharArray
  val y = x.toList.groupBy((c:Char) => c)
  val yListed = y.toList
  val ySorted = yListed.sortWith((a, b) => a._1 < b._1)
  val ys2 = yListed.sortWith( _._1 < _._1)
  val ys3 = yListed.sortBy(_._1)
  val ys4 = yListed.sortBy{ case (c, cList) => c }

  val sameAsY = x.toList.groupBy((x) => x)
  val sameAgain = x.toList.groupBy(c => c)

  val z = y.toList map { case(c, lc) => (c, lc.length)}
  val zAgain = sameAgain.toList map { case(c, lc) => (c, lc.length)}

  val ccdict = dictionary.reduceLeft(_ + _)

  def ocs(w:Word): Occurrences =
    w.toLowerCase.toList.groupBy((c:Char) => c).toList.sortBy{ case(c, cList) => c} map { case(c, lc) => (c, lc.length)}



  val bb = for ( w <- dictionary) yield (ocs(w), w)
  val cc = bb.groupBy{ case(o, w) => o }
  val ccr = cc map { case(o, listOfOWTuples:List[(Occurrences, Word)]) => (o, listOfOWTuples.foldLeft(List[Word]())((accum, owTuple) => owTuple._2::accum))

}