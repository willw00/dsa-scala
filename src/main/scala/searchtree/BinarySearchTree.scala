package searchtree

import common.Node

case class TreeNode(value: Int, left: Option[TreeNode], right: Option[TreeNode]) extends Node[Int] {
  def insert(node: TreeNode): TreeNode = {
    if (node.value < value) {
      left.map(l => TreeNode(value, Some(l.insert(node)), right)).getOrElse(TreeNode(value, Some(node), right))
    }
    else {
      right.map(r => TreeNode(value, left, Some(r.insert(node)))).getOrElse(TreeNode(value, left, Some(node)))
    }
  }
}

object BinarySearchTree {
  def empty = BinarySearchTree(None)
}

case class BinarySearchTree(root: Option[TreeNode]) {

  def insert(node: TreeNode): BinarySearchTree = {
    new BinarySearchTree(root.map { r =>
      r.insert(node)
    }.orElse(Some(node))
    )
  }

  def insert(value: Int): BinarySearchTree = insert(TreeNode(value, None, None))
}
