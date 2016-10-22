def digits(n: Int): Set[Int] = {
  def helper(n: Int, result: Set[Int]): Set[Int] = {
    if (n == 0) result else helper(n / 10, result + n % 10)
  }
  helper(n, Set.empty)
}

def findNumber(n: Int): Int = {
  if (
    digits(n * 2) == digits(n * 3) &&
      digits(n * 2) == digits(n * 4) &&
      digits(n * 2) == digits(n * 5) &&
      digits(n * 2) == digits(n * 6)
  ) n
  else findNumber(n + 1)
}

findNumber(1)
