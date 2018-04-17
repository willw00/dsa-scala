package searchtree

import common.Node

case class TreeNode(value: Int, left: Option[TreeNode], right: Option[TreeNode]) extends Node[Int] {
  def isLeaf = left.isEmpty && right.isEmpty

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

  def replace(toReplace: TreeNode, replacement: TreeNode): TreeNode = {
    if (toReplace == this) replacement
    else if (toReplace.value < value) left.map(_.replace(toReplace, replacement)).getOrElse(insert(replacement))
    else right.map(_.replace(toReplace, replacement)).getOrElse(insert(replacement))
  }

  def remove(toRemove: TreeNode, toRemoveParent: TreeNode): TreeNode = {
    if (this == toRemoveParent) {
      if (toRemoveParent.left.contains(toRemove) && toRemove.isLeaf) toRemoveParent.copy(left = None)
      else if (toRemoveParent.right.contains(toRemove) && toRemove.isLeaf) toRemoveParent.copy(right = None)
      else if (toRemoveParent.left.contains(toRemove) && toRemove.left.isEmpty && toRemove.right.isDefined) {
        toRemoveParent.copy(left = toRemove.right)
      }
      else if (toRemoveParent.right.contains(toRemove) && toRemove.left.isEmpty && toRemove.right.isDefined) {
        toRemoveParent.copy(right = toRemove.right)
      }
      else if (toRemoveParent.left.contains(toRemove) && toRemove.left.isDefined && toRemove.right.isEmpty) {
        toRemoveParent.copy(left = toRemove.left)
      }
      else if (toRemoveParent.right.contains(toRemove) && toRemove.left.isDefined && toRemove.right.isEmpty) {
        toRemoveParent.copy(right = toRemove.left)
      }
      else {
        val largest = toRemove.left.get.findLargest
        val largestParent = findParent(toRemove.left.get, largest.value).get
        val largestRemoved = largestParent.remove(largest, largestParent)
        toRemoveParent.copy(
          toRemoveParent.value,
          Some(toRemove.copy(value = largest.value)),
          toRemove.right.map(_.replace(largestParent, largestRemoved)))
      }
    }
    else if (toRemoveParent.value < value) left.get.remove(toRemove, toRemoveParent)
    else right.get.remove(toRemove, toRemoveParent)
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

  def remove(value: Int): BinarySearchTree = {
    if (root.isEmpty) BinarySearchTree.empty
    else {
      val r = root.get
      r.findNode(value).flatMap { nodeToRemove =>
        findParent(value).map { toRemoveParent =>
          val newRoot = r.remove(nodeToRemove, toRemoveParent)
          BinarySearchTree(Some(newRoot))
        }
      }.getOrElse(BinarySearchTree.empty)
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
    else {
      val r = root.get
      val newRoot = r.delete(r.value, None)
      BinarySearchTree(newRoot)
    }
  }

  def ==(other: BinarySearchTree) = this.toString == other.toString
}
