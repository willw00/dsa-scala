package searchtree

import common.Node

case class TreeNode[T <: Comparable[T]](value: T, left: Option[TreeNode[T]], right: Option[TreeNode[T]]) extends Node[T] {
  def insert(node: TreeNode[T]): TreeNode[T] = {
    if (node.value.compareTo(value) < 0) {
      left.map(l => l.insert(node)).getOrElse(TreeNode(value, Some(node), right))
    }
    else {
      right.map(r => r.insert(node)).getOrElse(TreeNode(value, left, Some(node)))
    }
  }
}

class BinarySearchTree[T](root: Option[TreeNode[T]]) {

  def insert(node: TreeNode[T]): BinarySearchTree[T] = {
    new BinarySearchTree(root.map { r =>
      r.insert(node)
    }.orElse(Some(node))
    )
  }

  def insert(value: T) = insert(TreeNode(value, None, None))
}
