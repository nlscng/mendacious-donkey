package com.mendaciousdonkey.interviewprep.lists

import scala.annotation.tailrec

sealed abstract class RList[+T]:
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def headOption: Option[T]
  def prepend[S >: T](elem: S): RList[S] = new Cons(elem, this)


case object RNil extends RList[Nothing]:
  override def head: Nothing = throw new NoSuchElementException()
  override def tail: RList[Nothing] = throw new NoSuchElementException()
  override def isEmpty: Boolean = true
  override def headOption: Option[Nothing] = None
  override def toString: String = "[]"

case class Cons[+T](override val head: T, override val tail: RList[T]) extends RList[T]:
  override def isEmpty: Boolean = false
  override def headOption: Option[T] = Option(head)

  override def toString: String =
    @tailrec
    def toStringTailRec(remaining: RList[T], result: String): String =
      if remaining.isEmpty then result
      else if remaining.tail.isEmpty then s"$result${remaining.head}"
      else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")

    "[" + toStringTailRec(this, "") + "]"

object ListProblems extends App {
  private val aSmallList = Cons(1, Cons(2, Cons(3, Cons(4, RNil))))
  println(aSmallList) // [1, 2, 3, 4]
}
