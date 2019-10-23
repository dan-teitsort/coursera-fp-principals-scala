object worksheet6Huh {

  def subsets(chars: List[Char]): List[List[Char]] = {

    println(f"?????????????????????????????? chars is $chars")

  val result =
      chars match {
        case Nil => List(List())
        case x::Nil => List(List(x))
        case x::xs => subsets(List(x)):::subsets(xs)
      }

    println(f"_____________________________ result is $result")
    result
  }


  val input = List ('a', 'b', 'c')

  val mySubsets = subsets(input)
  //for (r <- mySubsets) println(f"r is $r")
  println(f"mySubsets ========== $mySubsets")

  for (subset <- mySubsets) println(f"********* subset=$subset")
}