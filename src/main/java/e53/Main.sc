val LIMIT: BigDecimal = 1000000
def fac(n: BigDecimal): BigDecimal = {
  if (n <= 1) 1 else n * fac(n - 1)
}
def chose(n: Int, k: Int): BigDecimal = fac(BigDecimal(n)) / (fac(BigDecimal(k)) * fac(BigDecimal(n - k)))

def findNumberOfChoseGreaterThan1M(n: Int): Int = {
  if (chose(n, n / 2) < LIMIT) 0
  def helper(n: Int, k: Int, accumulator: Int): Int = {
    if (chose(n, k) < LIMIT) accumulator
    else {
      helper(n, k - 1, accumulator + 1)
    }
  }
  val helperVal: Int = helper(n, n / 2, 0) * 2
  if (n % 2 != 0 || helperVal == 0) helperVal else helperVal - 1
}

(1 to 100).map(findNumberOfChoseGreaterThan1M).sum