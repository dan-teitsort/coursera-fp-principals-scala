package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  //@tailrec
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else //if (r > 1)
      pascal(c-1, r-1) + pascal(c, r-1)

  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def closesCleanly(pendingOpens: Int, chars: List[Char]): Boolean = {

      if (chars.isEmpty) {
        pendingOpens == 0
      } else {
        val newPO = chars.head match {
          case '(' => pendingOpens + 1
          case ')' => pendingOpens - 1
          case _ => pendingOpens
        }

        if (newPO < 0) false else closesCleanly(newPO, chars.tail)
      }
    }

    closesCleanly(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(changeDue: Int, denominations: List[Int]): Int = {

    def helper(changeDue: Int, denominations:List[Int]): Int = {
      
      if (changeDue == 0)
        1
      else if (denominations.isEmpty && changeDue >= 1)
        0
      else if (changeDue < 0)
        0
      else helper(changeDue, denominations.tail) + helper(changeDue - denominations.head, denominations)
    }

    helper(changeDue, denominations)
  }
}
