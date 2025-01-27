package com.mendaciousdonkey.practice.lists

import scala.annotation.tailrec

sealed abstract class RList[+T]:
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def headOption: Option[T]
  def prepend[S >: T](elem: S): RList[S] = new ::(elem, this)
  def ::[S >: T](elem: S): RList[S] = new::(elem, this)
  def apply(index: Int): T
  def applyOption(index: Int): Option[T]
  def length: Int
  def reverse: RList[T]
  def ++[S >: T](other: RList[S]): RList[S]
  def removeAt(index: Int): RList[T]
  def map[S](f: T => S): RList[S]
  def flatmap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]
  def runLengthEncoding: RList[(T, Int)]

case object RNil extends RList[Nothing]:
  override def head: Nothing = throw new NoSuchElementException()
  override def tail: RList[Nothing] = throw new NoSuchElementException()
  override def isEmpty: Boolean = true
  override def headOption: Option[Nothing] = None
  override def toString: String = "[]"
  override def apply(index: Int): Nothing = throw new NoSuchElementException()
  override def applyOption(index: Int): Option[Nothing] = None
  override def length: Int = 0
  override def reverse: RList[Nothing] = this
  override def ++[S >: Nothing](other: RList[S]): RList[S] = other
  override def removeAt(index: Int): RList[Nothing] = RNil
  override def map[S](f: Nothing => S): RList[S] = RNil
  override def flatmap[S](f: Nothing => RList[S]): RList[S] = RNil
  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil
  override def runLengthEncoding: RList[Nothing] = RNil

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T]:
  override def isEmpty: Boolean = false
  override def headOption: Option[T] = Option(head)
  override def toString: String =
    @tailrec
    def toStringTailRec(remaining: RList[T], result: String): String =
      if remaining.isEmpty then result
      else if remaining.tail.isEmpty then s"$result${remaining.head}"
      else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")

    "[" + toStringTailRec(this, "") + "]"

  override def apply(index: Int): T =
    @tailrec
    def applyTailRec(remaining: RList[T], curIndex: Int): T =
      if remaining.isEmpty then throw new IndexOutOfBoundsException(s"Index $index is out of bound.")
      else if index == curIndex then remaining.head
      else applyTailRec(remaining.tail, curIndex + 1)

    if index >= 0 then applyTailRec(this, 0)
    else throw new NoSuchElementException()

  override def applyOption(index: Int): Option[T] =
    @tailrec
    def applyOptionTailRec(remaining: RList[T], curIndex: Int): Option[T] =
      if remaining.isEmpty then None
      else if index == curIndex then remaining.headOption
      else applyOptionTailRec(remaining.tail, curIndex + 1)

    applyOptionTailRec(this, 0)

  override def length: Int =
    @tailrec
    def lengthTailRec(remaining: RList[T], count: Int): Int =
      if remaining.isEmpty then count
      else lengthTailRec(remaining.tail, count + 1)

    lengthTailRec(this, 0)
  override def reverse: RList[T] =
    @tailrec
    def reverseTailRec(remaining: RList[T], acc: RList[T]): RList[T] =
      if remaining.isEmpty then acc
      else reverseTailRec(remaining.tail, remaining.head :: acc)
    reverseTailRec(this, RNil)

  override def ++[S >: T](other: RList[S]): RList[S] =
    @tailrec
    def recurse(remaining: RList[S], acc: RList[S]): RList[S] =
      if remaining.isEmpty then acc
      else recurse(remaining.tail, remaining.head :: acc)

    recurse(this.reverse, other)

  override def removeAt(index: Int): RList[T] =
    @tailrec
    def removeAtTailRec(remaining: RList[T], count: Int, acc: RList[T]): RList[T] =
      if count == index then remaining.tail.reverse ++ acc
      else removeAtTailRec(remaining.tail, count + 1, remaining.head :: acc)

    if index >= this.length || index < 0 then this
    else removeAtTailRec(this, 0, RNil).reverse

  override def map[S](f: T => S): RList[S] =
    @tailrec
    def mapTailRec(remaining: RList[T], acc: RList[S]): RList[S] =
      if remaining.isEmpty then acc.reverse
      else mapTailRec(remaining.tail, f(remaining.head) :: acc)

    mapTailRec(this, RNil)

  override def flatmap[S](f: T => RList[S]): RList[S] =
    @tailrec
    def flatmapTailRec(remaining: RList[T], acc: RList[S]): RList[S] =
      if remaining.isEmpty then acc.reverse
      else flatmapTailRec(remaining.tail, f(remaining.head).reverse ++ acc)

    flatmapTailRec(this, RNil)

  override def filter(f: T => Boolean): RList[T] =
    @tailrec
    def filterTailRec(remaining: RList[T], acc: RList[T]): RList[T] =
      if remaining.isEmpty then acc.reverse
      else {
        val out = f(remaining.head)
        if out then filterTailRec(remaining.tail, remaining.head :: acc)
        else filterTailRec(remaining.tail, acc)
      }

    filterTailRec(this, RNil)

  override def runLengthEncoding: RList[(T, Int)] =
    @tailrec
    def rleTailRec(remaining: RList[T], currentTuple: (T, Int), acc: RList[(T, Int)]): RList[(T, Int)] =
      if remaining.isEmpty && currentTuple._2 == 0 then acc
      else if remaining.isEmpty then currentTuple :: acc
      else if remaining.head == currentTuple._1 then rleTailRec(remaining.tail, (currentTuple._1, currentTuple._2 + 1), acc)
      else rleTailRec(remaining.tail, (remaining.head, 1), currentTuple :: acc)

    rleTailRec(this.tail, (this.head, 1), RNil).reverse

object ListProblems extends App {
//  private val aSmallList = ::(1, ::(2, ::(3, ::(4, RNil))))
  private val aSmallList = 1 :: 2 :: 3 :: 4 :: RNil
  println(aSmallList) // [1, 2, 3, 4]

  println(s"The element at index 3: ${aSmallList.apply(3)}") // 4
//  println(aSmallList(-1)) // exception

  println(aSmallList.applyOption(2)) // Some(3)
  println(aSmallList.applyOption(42)) // None
  println(aSmallList.applyOption(-3)) // None


  // length
  println(s"Length: ${aSmallList.length}") // 4

  // reverse
  println(s"Reverse: ${aSmallList.reverse}")

  // concate
  private val anotherSmallList = 6 :: 7 :: 8 :: 9 :: RNil
  println(s"Concate aSmallList: $aSmallList with anotherSmallList: $anotherSmallList, we get: ${aSmallList ++ anotherSmallList}")


  // remove at k
  println(s"remove at k with k being 3: ${aSmallList.removeAt(2)}") // [1, 2, 4]
  println(s"remove at k with k being -2: ${aSmallList.removeAt(-2)}") // no change
  println(s"remove at k with k being 4: ${aSmallList.removeAt(4)}") // no change
  println(s"remove at k with k being 0: ${aSmallList.removeAt(0)}") // [2, 3, 4]

  // map
  println(s"map: ${aSmallList.map(_ + 0.5)}")

  // flatmap
  println(s"flatmap: ${aSmallList.flatmap(x => x :: x*x :: RNil)}")

  // run length encoding
  private val listWithDuplicates = 1 :: 1 :: 1 :: 2 :: 2 :: 3 :: 3 :: 3 :: 42 :: 42 :: RNil
  println(s"run length encoding: ${listWithDuplicates.runLengthEncoding}")
}
