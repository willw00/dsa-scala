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

  def delete(toRemove: TreeNode, toRemoveParent: TreeNode): TreeNode = {
    println(s"To Remove: $toRemove.  Parent: $toRemoveParent")
    if (this == toRemoveParent) {
      println(s"On the parent")
      if (toRemoveParent.left.contains(toRemove) && toRemove.isLeaf) {
        println(s"ToRemove is leaf, is Left")
        toRemoveParent.copy(left = None)
      }
      else if (toRemoveParent.right.contains(toRemove) && toRemove.isLeaf) {
        println(s"ToRemove is leaf, is Right")
        toRemoveParent.copy(right = None)
      }
      else if (toRemoveParent.left.contains(toRemove) && toRemove.left.isEmpty && toRemove.right.isDefined) {
        println(s"ToRemove is left, left empty")
        toRemoveParent.copy(left = toRemove.right)
      }
      else if (toRemoveParent.right.contains(toRemove) && toRemove.left.isEmpty && toRemove.right.isDefined) {
        println(s"ToRemove is right, left empty")
        toRemoveParent.copy(right = toRemove.right)
      }
      else if (toRemoveParent.left.contains(toRemove) && toRemove.left.isDefined && toRemove.right.isEmpty) {
        println(s"ToRemove is left, right empty")
        toRemoveParent.copy(left = toRemove.left)
      }
      else if (toRemoveParent.right.contains(toRemove) && toRemove.left.isDefined && toRemove.right.isEmpty) {
        println(s"ToRemove is right, right empty")
        toRemoveParent.copy(right = toRemove.left)
      }
      else {
        println(s"Else")
        val largest = toRemove.left.get.findLargest
        println(s"Largest: $largest")
        val largestParent = findParent(toRemove.left.get, largest.value).get
        println(s"Largest Parent: $largestParent")
        val largestRemoved = largestParent.delete(largest, largestParent)
        println(s"LargestRemoved: $largestRemoved")
        val newRight = Some(largestRemoved.copy(value = largest.value))
        println(s"NewRight: $newRight")
        this.copy(
          value,
          left,
          newRight)
      }
    }
    else if (toRemoveParent.value < value) {
      println(s"Parent is lower")
      this.copy(left = left.map(_.delete(toRemove, toRemoveParent)))
    }
    else {
      println(s"Parent is larger")
      this.copy(right = right.map(_.delete(toRemove, toRemoveParent)))
    }
  }

  def findLargest: TreeNode = {
    right.map(_.findLargest).getOrElse(this)
  }

  def findSmallest: TreeNode = {
    left.map(_.findSmallest).getOrElse(this)
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

  def findSmallest: Option[Int] = {
    root.map(_.findSmallest.value)
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
    else {
      val r = root.get
      r.findNode(value).flatMap { nodeToRemove =>
        findParent(value).map { toRemoveParent =>
          val newRoot = r.delete(nodeToRemove, toRemoveParent)
          BinarySearchTree(Some(newRoot))
        }.orElse {
          r.left.map { l =>
            val largest = l.findLargest
            val newRoot =
              if (l != largest) largest.copy(left = Some(largest.copy(largest.value, r.left, r.right)))
              else l.copy(right = r.right)
            BinarySearchTree(Some(newRoot))
          }
        }
      }.getOrElse(BinarySearchTree.empty)
    }
  }

  def ==(other: BinarySearchTree) = this.toString == other.toString
}
