package linkedlist

import common.Node

trait LinkedList[T, N <: Node[T], L <: LinkedList[T, N, _]] {
  def head: Option[N]
  def tail: Option[N]

  //Append to front of list
  def add(node: N): L
  //Remove from head of list
  def remove: L
  def contains(value: T): Boolean

  def isEmpty: Boolean
}
