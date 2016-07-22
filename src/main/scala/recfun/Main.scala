package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }


    countChange(100, List(3, 6, 1, 2, 36, 44, 7, 7, 2, 7, 3, 12, 35, 10
    ))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1 else if (r == 0) 0 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def needClose(chars: List[Char], times: Int): Boolean = {
      if (chars.isEmpty) times == 0
      else if (chars.head == '(') needClose(chars.tail, times + 1)
      else if (chars.head == ')') times > 0 && needClose(chars.tail, times - 1)
      else needClose(chars.tail, times)
    }

    needClose(chars, 0)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money > 0 && coins.nonEmpty) {
      countChange(money - coins.head, coins) +
        countChange(money, coins.tail)
    }
    else
      0
  }
}

