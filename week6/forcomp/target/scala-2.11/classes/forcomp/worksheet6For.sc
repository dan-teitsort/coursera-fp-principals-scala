object  worksheet6For {

  def makeList(input: List[Char]): List[List[Char]] = {
    println(f"making list wrapper around $input")
    List(input)
  }

  def subsets(chars: List[Char]): List[List[Char]] = {

    println(f"?????????????????????????????? chars is $chars")

    if (false && chars.isEmpty) {
      println(f"Oh no... chars is empty, see: $chars")
      List()
    }
    else {
//      val tmp = for {
//        split <- 1 to chars.length
//        leftMost <- makeList(chars.take(split))
//        rest <- subsets(chars.drop(split))
//      } yield {
//        println(f">>>> yielding split=$split, leftMost=$leftMost")
//        val yieldResult = leftMost ::: rest
//        println(f"yieldResult is $yieldResult")
//        yieldResult
//      }
      val fff = for {
        split <- 1 to chars.length
        leftMost <- makeList(chars.take(split))
        //if (split < (chars.length - 1))
        rest <- subsets(chars.drop(split))
      } yield leftMost :: rest

      println(f"....................................fff is $fff")
      val tmp2 = fff map { case(loc:List[Char]) => loc}
      val result = tmp2.toList
      println(f"arg was $chars and result will be $result")
      result
    }
  }


  val input = List ('a', 'b', 'c')

  val mySubsets = subsets(input)
  for (r <- mySubsets) println(f"r is $r")
}