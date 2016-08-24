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
    if (c == 0 || r == c || r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def innerfunction(chars: List[Char], account: Int, lastone: Boolean): Boolean = {
      if (chars.isEmpty && account == 0 && lastone) true
      else if (chars.isEmpty & account == 0 && !lastone) false
      else if (chars.isEmpty && account !=0) false
      else if (chars.head == '(') innerfunction(chars.tail, account + 1,false)
      else if (chars.head == ')') innerfunction(chars.tail, account - 1,true)
      else if (!chars.isEmpty) innerfunction(chars.tail,account,lastone)
      else false
    }
    innerfunction(chars, 0,true)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
