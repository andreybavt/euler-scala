def isPalindrome(n: BigDecimal): Boolean = {
  val string: String = n.toString
  n.toString == string.reverse
}

def reverse(n: BigDecimal): BigDecimal = {
  BigDecimal(n.toString.reverse)
}

def isLychrel(n: Int): Boolean = {
  def helper(num: BigDecimal, iter: Int): Boolean = {
    if (iter > 50) true
    else {
      val sum: BigDecimal = num + reverse(num)
      if (isPalindrome(sum)) {
        false
      }
      else
        helper(sum, iter + 1)
    }
  }
  helper(n, 0)
}

(1 to 10000).count(isLychrel)






