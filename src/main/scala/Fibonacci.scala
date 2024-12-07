import scala.annotation.tailrec

object Fibonacci {

  private def fibonacciBruteForce(n: Int): Int =
    if (n < 0) throw IllegalAccessException("n needs to be greater than or equal to zero.")

    if (n == 0 || n == 1) n
    else fibonacciBruteForce(n - 1) + fibonacciBruteForce(n - 2)

  private def testBruteForce(): Unit =
    println(fibonacciBruteForce(0)) // 0
    println(fibonacciBruteForce(1)) // 1
    println(fibonacciBruteForce(2)) // 1
    println(fibonacciBruteForce(3)) // 2
    println(fibonacciBruteForce(4)) // 3
    println(fibonacciBruteForce(5)) // 5
    println(fibonacciBruteForce(6)) // 8
    println(fibonacciBruteForce(12)) // 144

  private def fibonacciVer2(n: Int): Int = {
    if (n < 0) throw IllegalArgumentException("n needs to be greater than or equal to zero")
    if (n == 0 || n == 1) n
    else {
      @tailrec
      def fiboInner(k: Int, memo: List[Int]): Int =
//        println(s"k is ${k}, size of memo is ${memo.size}, memo is : ${memo}")
        if (memo.size > 20) 5000

        else if (k == memo.size) memo.reverse.take(2).sum
        else {
          val last = memo.last
          val secondLast = memo.init.last
          fiboInner(k, memo ::: List(secondLast + last))
        }

      fiboInner(n, List(0, 1))
    }
  }

  private def testVer2(): Unit =
    println("testing version 2, dynamic programming maybe")
    println(fibonacciVer2(0)) // 0
    println(fibonacciVer2(1)) // 1
    println(fibonacciVer2(2)) // 1
    println(fibonacciVer2(3)) // 2
    println(fibonacciVer2(4)) // 3
    println(fibonacciVer2(5)) // 5
    println(fibonacciVer2(6)) // 8
    println(fibonacciVer2(12)) // 144

  @main
  def hello(): Unit =
    testBruteForce()

    testVer2()
}
