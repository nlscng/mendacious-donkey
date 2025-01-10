import scala.annotation.tailrec
import scala.math.Ordering.Implicits.infixOrderingOps

object BinarySearch {

  private def binarySearch[T](seq: IndexedSeq[T], target: T)(implicit numeric: Numeric[T]): Int =

    @tailrec
    def recur(low: Int, high: Int): Int =
      if (low > high) -1
      else {
        val midIdx: Int = (low + high) / 2
        val midVal: T = seq(midIdx)
        if (midVal == target) midIdx
        else if (midVal < target) recur(midIdx+1, high)
        else recur(low, midIdx - 1)
      }

    recur(0, seq.size - 1)


  @main
  private def binarySearchMain(): Unit =
    println("hello")

    val test1: IndexedSeq[Int] = IndexedSeq[Int](1, 2, 3, 5, 7, 11, 13, 17, 19)
    println(binarySearch(test1, 13)) //6
    println(binarySearch(test1, 8)) // -1
    println(binarySearch(test1, 1)) // 0
    println(binarySearch(test1, 19)) // 8
}
