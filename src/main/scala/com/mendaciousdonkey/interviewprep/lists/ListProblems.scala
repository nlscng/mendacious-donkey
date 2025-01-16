package com.mendaciousdonkey.interviewprep.lists

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

case object RNil extends RList[Nothing]:
  override def head: Nothing = throw new NoSuchElementException()
  override def tail: RList[Nothing] = throw new NoSuchElementException()
  override def isEmpty: Boolean = true
  override def headOption: Option[Nothing] = None
  override def toString: String = "[]"
  override def apply(index: Int): Nothing = throw new NoSuchElementException()
  override def applyOption(index: Int): Option[Nothing] = None

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


object ListProblems extends App {
//  private val aSmallList = ::(1, ::(2, ::(3, ::(4, RNil))))
  private val aSmallList = 1 :: 2 :: 3 :: 4 :: RNil
  println(aSmallList) // [1, 2, 3, 4]

  println(aSmallList.apply(3)) // 4
//  println(aSmallList(-1)) // exception

  println(aSmallList.applyOption(2)) // Some(3)
  println(aSmallList.applyOption(42)) // None
  println(aSmallList.applyOption(-3)) // None
}
