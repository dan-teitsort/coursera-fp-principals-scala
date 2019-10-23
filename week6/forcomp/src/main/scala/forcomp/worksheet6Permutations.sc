object worksheet6Permutations {



  val letters = List('a', 'b', 'c')

  val anyOrder = for {
    l <- letters
    x <- letters
  } yield (l, x)

  val alphaOrder = for {
    l <- letters
    x <- letters.dropWhile(p => p <= l)
  } yield (l, x)

  // Goal:
  //  a
  //  ab
  //  abc
  //  ac
  //
  //  b
  //  bc
  //
  //  c

  def permutations(chars: List[Char]): List[String] = {

    chars match {
      case Nil => List()
      case x::Nil => List(x.toString())
      case x::xs => {
        val ps = permutations(xs)
        println(f"x is $x and xs is $xs and ps is $ps")
        ps ::: ps map (p => x.toString() + p)
      }
    }
  }

  def perm2(chars: List[Char]): List[String] = {

    chars match {
      case Nil => List()
      case y :: Nil => List(y.toString())
      case x :: xs => {

        val maxStemLength = xs.length
        for {
          stemLength <- (0 until maxStemLength)
        } yield (x, xs.take(stemLength)) //map ((c, cList) => )
        List("hi")
      }
    }
  }

  //permutations(letters)

  //print(ppp)

  perm2(letters)

}