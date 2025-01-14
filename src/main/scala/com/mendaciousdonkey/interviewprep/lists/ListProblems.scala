package com.mendaciousdonkey.interviewprep.lists

sealed abstract class RList[+A]:
  def head: A
  def tail: RList[A]
  def isEmpty: Boolean
  def headOption: Option[A]

case object RNil extends RList[Nothing]:
  override def head: Nothing = throw new NoSuchElementException()
  override def tail: RList[Nothing] = throw new NoSuchElementException()
  override def isEmpty: Boolean = true
  override def headOption: Option[Nothing] = None

object ListProblems extends App {

}
