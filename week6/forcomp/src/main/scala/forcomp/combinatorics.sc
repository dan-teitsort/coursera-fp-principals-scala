object Combinatorics {

  def myCombinations[A](s: Seq[A]) = for {i <- 0 to s.length; j <- s combinations i} yield j

  val letters = List('a', 'b', 'c')

  val letterCombinations = myCombinations(letters)
  val count = letterCombinations.size

  val letterCombinationsAsSet = letterCombinations.toSet
  val countAsSet = letterCombinationsAsSet.size

  def wrong[A](s: Seq[A]) = for {i <-1 to s.length; j <- s combinations i} yield j

  val wrongCombinations = wrong(letters)
  val wrongCount = wrongCombinations.size
  
}