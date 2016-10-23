def digits(n: Long): List[Int] = {
  def helper(num: Long, result: List[Int]): List[Int] =
    if (num == 0 && result.nonEmpty) result.reverse
    else helper(num / 10, result :+ (num % 10).toInt)
  helper(n, List.empty)
}

def nextNum(n: Int) = digits(n).map(Math.pow(_, 2)).sum.toInt

def arrivesTo(num: Int): Int = {
  val next: Int = nextNum(num)
  if (next == 1 || next == 89) next
  else arrivesTo(next)
}

(1 to 10000000).map(arrivesTo).count(_ == 89)


