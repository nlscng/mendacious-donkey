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
}
