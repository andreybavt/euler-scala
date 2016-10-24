import scala.io.Source

val arabtoRomanMap: Map[Int, String] = Map(
  0 -> "",
  1 -> "I",
  2 -> "II",
  3 -> "III",
  4 -> "IV",
  5 -> "V",
  6 -> "VI",
  7 -> "VII",
  8 -> "VIII",
  9 -> "IX",

  10 -> "X",
  100 -> "C",
  1000 -> "M",
  50 -> "L",
  500 -> "D"
)

val romanToArabMap: Map[String, Int] = arabtoRomanMap.map(_.swap)

def digits(n: Int): List[Int] = {
  def helper(n: Int, result: List[Int]): List[Int] =
    if (n == 0 && result.nonEmpty) result else helper(n / 10, result :+ (n % 10))
  helper(n, List.empty)
}

def promoteRoman(s: String): String = {
  def helper(l: List[String], result: List[String]): List[String] = {
    l match {
      case lh :: Nil => result :+ arabtoRomanMap(romanToArabMap(lh) * 10)
      case lh :: ls => (result :+ arabtoRomanMap(romanToArabMap(lh) * 10)) ::: helper(ls, result)
    }
  }
  helper(s.split("").toList, List.empty).foldRight("")((str, cur) => str + cur)
}
assert("MDCCCXC" == promoteRoman("CLXXXIX"))

digits(456)

def arabToRoman(n: Int): String = {
  def helper(digits: List[String], result: List[String]): List[String] = {
    digits match {
      case lh :: Nil => lh +: result
      case lh :: ls => helper(ls.map(promoteRoman), result) ::: (result :+ lh: List[String])
    }
  }
  val thousands: Int = Math.max((n / 1000) - 1, 0)

  (List.fill(thousands)("M") ::: helper(digits(n - thousands * 1000).map(arabtoRomanMap), List()))
    .foldRight("")((str, cur) => str + cur)
}

def invalidRomanToArab(n: String): Int = {
  def helper(digits: List[String], prevDigit: Int, result: Int): Int = {
    if (digits.isEmpty) result
    else {
      val digit: Int = romanToArabMap(digits.head)
      helper(digits.tail, digit, if (prevDigit <= digit) result + digit else result - digit)
    }
  }
  helper(n.split("").toList.reverse, 0, 0)
}

assert(arabToRoman(1995) == "MCMXCV")
assert(arabToRoman(1968) == "MCMLXVIII")
assert(arabToRoman(1) == "I")
assert(arabToRoman(10) == "X")
assert(arabToRoman(100) == "C")
assert(arabToRoman(1000) == "M")

val fileContents = Source.fromURL(getClass.getResource("/e89/romanNumbers")).getLines.toList
val result = fileContents.map(invalidNum => (invalidNum, invalidRomanToArab(invalidNum), arabToRoman(invalidRomanToArab(invalidNum))))

result.map(el=> el._1.length - el._3.length).sum