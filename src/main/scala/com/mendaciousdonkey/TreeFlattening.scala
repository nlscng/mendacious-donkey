package com.mendaciousdonkey

import com.mendaciousdonkey.util.BinaryTreeNode

object TreeFlattening {

  private def flatten[A](root: BinaryTreeNode[A]): List[A] =
    def inner[A](node: BinaryTreeNode[A], acc: List[A]): List[A] =
      val fromLeft: List[A] = node.left.map(inner(_, acc)).getOrElse(Nil)
      val fromSelf: List[A] = node.value :: acc
      val fromRight: List[A] = node.right.map(inner(_, acc)).getOrElse(Nil)
      fromLeft ++ fromSelf ++ fromRight
    inner(root, Nil)

  def main(): Unit =
    val one = BinaryTreeNode(1, None, None)
    val four = BinaryTreeNode(4, None, None)
    val six = BinaryTreeNode(6, None, None)
    val nine = BinaryTreeNode(9, None, None)
    val three = BinaryTreeNode(3, Option(one), Option(four))
    val seven = BinaryTreeNode(7, None, Option(nine))
    val five = BinaryTreeNode(5, Option(three), Option(seven))

    print(flatten((five)))
}
