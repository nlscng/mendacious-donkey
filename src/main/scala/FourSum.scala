import scala.annotation.tailrec

object FourSum {
  def fourSum(numbers: List[Int], target: Int): List[Int] =
    if (numbers.size < 4) Nil
    else {
      val sorted = numbers.sorted
      val head = sorted.head
      val threeSumRes = threeSum(sorted.tail, target - head)
      if (threeSumRes.size == 3) List(head, threeSumRes.head, threeSumRes.tail.head, threeSumRes.takeRight(1).head)
      else fourSum(sorted.tail, target)
    }

  @tailrec
  private def threeSum(numbers: List[Int], target: Int): List[Int] =
    if (numbers.size < 3) Nil
    else {
      val head = numbers.head
      val twoSumRes = twoSum(numbers.tail, target - head)
      if (twoSumRes.size == 2) List(head, twoSumRes.head, twoSumRes.tail.head)
      else threeSum(numbers.tail, target)
    }

  @tailrec
  private def twoSum(numbers: List[Int], target: Int): List[Int] =
    if (numbers.size < 2) Nil
    else {
      val left = numbers.head
      val right = numbers.takeRight(1).head
      val curSum: Int = left + right

      if (curSum == target) List(left, right)
      else if (curSum < target) twoSum(numbers.tail, target)
      else twoSum(numbers.init, target)
    }

  @main def testFourSum(): Unit =
    println(twoSum(Nil, 42))
    println(twoSum(List(3), 42))
    println(twoSum(List(2, 3), 5))
    println(twoSum(List(2, 3, 14), 17))

    println(threeSum(Nil, 42))
    println(threeSum(List(3, 5, 8), 10))
    println(threeSum(List(3, 5, 8), 16))

    println(fourSum(List(1, 2, 3, 4, 5, 9, 19, 12, 12, 19), 40))
    println(fourSum(List(2, 19, 3, 4, 12, 5, 1, 9, 19, 12), 40))
}
