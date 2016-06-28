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
    // sum of the two numbers above it.
    if(r == 0) {
      1
    } else {
      if(c == 0) {
        1
      } else if(c == r) {
        1
      } else {
        (pascal(c-1, r-1) + pascal(c, r-1))
      }
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    if(chars.isEmpty) {
      true
    } else {
      if(balanceWithCount(chars, 0) == 0) {
        true
      } else {
        false
      }
    }
  }

  def balanceWithCount(chars: List[Char], count: Int): Int = {
    if(chars.isEmpty) {
      count
    } else {
      if(chars.head.equals('(')) {
        balanceWithCount(chars.tail, count+1)
      } else if(chars.head.equals(')')) {
        if(count == 0) {
          -1
        } else {
          balanceWithCount(chars.tail, count - 1)
        }
      } else {
        balanceWithCount(chars.tail, count)
      }
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(coins.isEmpty) {
      0
    } else if(money < 0) {
      0
    } else if(money == 0) {
      1
    } else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
}
