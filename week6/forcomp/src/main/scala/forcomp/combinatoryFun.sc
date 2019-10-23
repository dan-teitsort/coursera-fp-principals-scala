package forcomp

object CombosFun {


    def combinations(chars: List[Char]): List[List[Char]] = {
      //
      //      def xcombinations(n: Int): List[List[A]] =
      //        if (n > xsize) Nil
      //        else l match {
      //          case _ :: _ if n == 1 =>
      //            l.map(List(_))
      //          case hd :: tl =>
      //            tl.xcombinations(n - 1).map(hd :: _) ::: tl.xcombinations(n)
      //          case _ => Nil

      def ncombinations(n: Int, choices: List[Char]): List[List[Char]] = {

        if (n > choices.size) Nil
        else choices match {
          case _ :: _ if n == 1 => choices.map(List(_))
          case hd :: tl => ncombinations(n - 1, tl).map(hd :: _) ::: ncombinations(n, tl)
          case _ => Nil
        }
      }

      ncombinations(chars.size, chars)
    }


    val input = List('a', 'b', 'c')

    val combos = combinations(input)
    //for (r <- mySubsets) println(f"r is $r")
    println(f"combos ========== $combos")

    val csize = combos.size
    println(f"combos.size = $csize")

    for (aCombo <- combos) {
      println(f"********* aCombo=$aCombo")
    }
  }
