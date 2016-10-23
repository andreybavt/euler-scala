def getLast10Digits(n: Long): Long = {
  val LIMIT: Long = 10000000000L
  if (n < LIMIT) n else n % LIMIT
}

def last10DigitsOfPower(n: Int): Long = {
  def helper(num: Long, pow: Int): Long = {
    if (pow >= n - 1) num
    else {
      helper(getLast10Digits(num * n), pow + 1)
    }
  }
  helper(n, 0)
}

getLast10Digits((1 to 1000).map(last10DigitsOfPower).sum)