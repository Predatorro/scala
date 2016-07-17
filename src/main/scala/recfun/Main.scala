package recfun

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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1 else if (r == 0) 0 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true
    else return chain(chars)

    def chain(chars: List[Char]): Boolean = {
      if (chars.isEmpty) true
      else if (chars.head == '(') needClose(chars.tail, 1)
      else if (chars.head == ')') false
      else chain(chars.tail)
    }

    def needClose(chars: List[Char], times: Int): Boolean = {
      if (times == 0) chain(chars)
      else if (chars.isEmpty) false
      else if (chars.head == '(') needClose(chars.tail, times + 1)
      else if (chars.head == ')') needClose(chars.tail, times - 1)
      else needClose(chars.tail, times)
    }

    true
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
}

