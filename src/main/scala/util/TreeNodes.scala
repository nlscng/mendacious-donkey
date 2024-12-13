package util

case class BinaryTreeNode[A](value: A, left: Option[BinaryTreeNode[A]], right: Option[BinaryTreeNode[A]])

case class TreeNode[A](value: A, children: List[TreeNode[A]])
