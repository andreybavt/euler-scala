import scala.collection.immutable.IndexedSeq

def isRightTriangle(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
  val zeroC1Square = x1 * x1 + y1 * y1
  val zeroC2Square = x2 * x2 + y2 * y2
  val c1C2Square = Math.pow(x1 - x2, 2) + Math.pow(y1 - y2, 2)
  val result: Boolean = (zeroC1Square == zeroC2Square + c1C2Square) ||
    (zeroC2Square == zeroC1Square + c1C2Square) ||
    (c1C2Square == zeroC1Square + zeroC2Square)
  result
}
val DIM = 50

val tuples: IndexedSeq[((Int, Int), (Int, Int))] = for {
  x1 <- 0 to DIM
  x2 <- 0 to DIM
  y1 <- 0 to DIM
  y2 <- 0 to DIM
  if (x1, y1) != (x2, y2) && (x1, y1) != (0, 0) && (x2, y2) != (0, 0) && isRightTriangle(x1, y1, x2, y2)
}
  yield ((x1, y1), (x2, y2))

val unique: List[((Int, Int), (Int, Int))] =
  tuples.foldLeft(List[((Int, Int), (Int, Int))]()) {
    (uniq, curr) =>
      if (uniq.contains(curr) || uniq.contains((curr._2, curr._1))) uniq
      else curr +: uniq
  }

unique.size
