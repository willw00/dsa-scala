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

  def findNode(v: Int): Option[TreeNode] = {
    if (value == v) Some(this)
    else if (v < value) {
      left.flatMap(_.findNode(v))
    }
    else right.flatMap(_.findNode(v))
  }

  def findParent(parent: TreeNode, v: Int): Option[TreeNode] = {
    if (value == v) Some(parent)
    else if (v < value) {
      left.flatMap(_.findParent(this, v))
    }
    else {
      right.flatMap(_.findParent(this, v))
    }
  }

  def delete(v: Int, parent: Option[TreeNode]): TreeNode = {
    if (value == v) parent.map(_.copy(left = left, right = right)).getOrElse(throw new Exception("Your tree has no root"))
    else if (v < value) {
      left.flatMap(_.delete(v, Some(this)))
    }
  }
}

object BinarySearchTree {
  def empty = BinarySearchTree(None)
}

case class BinarySearchTree(root: Option[TreeNode]) {

  def insert(node: TreeNode): BinarySearchTree = {
    new BinarySearchTree(
      root.map { r =>
        r.insert(node)
      }.orElse(Some(node))
    )
  }

  def insert(value: Int): BinarySearchTree = insert(TreeNode(value, None, None))

  def contains(value: Int): Boolean = {
    root.exists(_.findNode(value).isDefined)
  }

  def findParent(value: Int): Option[TreeNode] = {
    if (root.exists(_.value == value)) None
    else root.flatMap { r =>
      if (value < r.value) {
        r.left.flatMap(_.findParent(r, value))
      }
      else {
        r.right.flatMap(_.findParent(r, value))
      }
    }
  }

  def delete(value: Int): BinarySearchTree = {
    if (root.isEmpty) BinarySearchTree.empty
    else if (root.get.value == value) BinarySearchTree.empty
    else root.map { r =>
      BinarySearchTree(Some(r.delete(r, None)))
    }.getOrElse(this)
  }
}
