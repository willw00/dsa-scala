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

  def count: Int = {
    1 + left.map(_.count).getOrElse(0) + right.map(_.count).getOrElse(0)
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

  def delete(v: Int, parent: Option[TreeNode]): Option[TreeNode] = {
    if (value == v) {
      if (parent.isEmpty) {

      }
      parent.map(_.copy(left = left, right = right))
    }
    else if (v < value) {
      left.flatMap(_.delete(v, Some(this)))
    }
    else right.flatMap(_.delete(v, Some(this)))
  }

  def findLargest: TreeNode = {
    right.map(_.findLargest).getOrElse(this)
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

  def count: Int = {
    root.map(_.count).getOrElse(0)
  }

  def findLargest: Option[Int] = {
    root.map(_.findLargest.value)
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
    else if (root.get.value == value) {
      root.get.left.map { l =>
        val largest = l.findLargest
        val newLargest = largest.copy(left = l.delete(largest.value, None), right = root.get.right)
        BinarySearchTree(Some(newLargest))
      }.getOrElse(BinarySearchTree(root.get.right))
    }
    else root.map { r =>
      val newRoot = r.delete(r.value, None)
      BinarySearchTree(newRoot)
    }.getOrElse(this)
  }

  def ==(other: BinarySearchTree) = this.toString == other.toString
}
