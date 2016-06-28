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

  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (c, r) if ((c == 0) || (c == r)) => 1
    case (_, _) => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(pending: List[Char], stack: List[Char]): Boolean = pending match {
      case Nil => if (stack.isEmpty) true else false
      case h :: tail if (h == '(') => loop(tail, h :: stack)
      case h :: tail if (h == ')') => if (stack.isEmpty) false else loop(tail, stack.tail)
      case h :: tail => loop(tail, stack)
    }
    loop(chars, Nil: List[Char])
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(m: Int, c: List[Int]): Int = c match {
      case Nil => 0
      case h :: t if (money - h == 0) => 1
      case h :: t if (money - h < 0) => 0
      case _ => countChange(m - c.head, c) + countChange(m, c.tail)
    }
    count(money, coins.sorted)
  }
}