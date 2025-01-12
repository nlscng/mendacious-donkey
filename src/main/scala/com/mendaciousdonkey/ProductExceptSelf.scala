package com.mendaciousdonkey

object ProductExceptSelf {
  private def productExceptSelf(numbers: List[Int]): List[Int] =
    val numToTake = numbers.size
    val leftProduct: List[Int] = numbers.scanLeft(1) { (acc, ele) => acc * ele }
    val leftProductExceptSelf = leftProduct.take(numToTake)

    val rightProduct = numbers.scanRight(1) { (acc, ele) => acc * ele }
    val rightProductExceptSelf = rightProduct.takeRight(numToTake)

    leftProductExceptSelf.zip(rightProductExceptSelf).map(_ * _)


  private def productExceptSelfWithDivision(numbers: List[Int]): List[Int] = {
    val allProduct = numbers.product
    numbers.map(allProduct / _)
  }

  @main def testProductExceptSelf(): Unit =
    println(productExceptSelf(List(3, 4, 5)))
    println(productExceptSelfWithDivision(List(3, 4, 5)))
    println(productExceptSelf(List(3, 4, 5, 6)))
}
