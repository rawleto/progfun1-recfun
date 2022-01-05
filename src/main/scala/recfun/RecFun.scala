package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface :

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    def pascalHelper(acc: Int, c: Int, r: Int): Int = {
      if c > r then throw new IllegalArgumentException("The column number cannot be bigger than row")
      if c == 0 then 1
      else if c == -1 then 0
      else if c == r then 1
      else pascalHelper(acc, c, r - 1) + pascalHelper(acc, c - 1, r - 1)
    }

    pascalHelper(0, c, r)
    //        pascalHelper(0, 0) // 1                           // pascalHelper(0, -1) + pascalHelper(-1, -1)
    //
    //        pascalHelper(0, 1) // pascalHelper(0, 0) + 0      // pascalHelper(-1, 0) + pascalHelper(0, 0)
    //        pascalHelper(1, 1) // pascalHelper(0, 0) + 0      //  pascalHelper(0, 0)   + pascalHelper(1, 0)
    //
    //        pascalHelper(0, 2) // pascalHelper(0, 0) + 0      // pascalHelper(0, 1)   + pascalHelper(-1, 1)
    //        pascalHelper(1, 2) // pascalHelper(0, 1) + pascalHelper(1,1)
    //        pascalHelper(2, 2) // pascalHelper(0, 0) + 0
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balanceAcc(acc: Int, reduce: List[Char]): Boolean =
      if reduce.nonEmpty then
        if reduce.head == '(' then balanceAcc(acc + 1, reduce.tail)
        else if reduce.head == ')' then
          if acc > 0 then balanceAcc(acc - 1, reduce.tail)
          else false
        else balanceAcc(acc, reduce.tail)
      else acc == 0

    balanceAcc(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeAcc(acc: Int, money: Int, coins: List[Int]): Int = {
      if coins.isEmpty then 0
      else if money == 0 then acc + 1 // Found a solution
      else if money < 0 then acc // Not a solution
      else
        countChangeAcc(acc, money - coins.head, coins) + // Trying to add the same coin
          countChangeAcc(acc, money, coins.tail) // Trying to add another coin
    }

    if money == 0 then 0
    else countChangeAcc(0, money, coins)
  }
