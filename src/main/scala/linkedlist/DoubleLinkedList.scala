package linkedlist

import common.Node


object DoubleLinkedNode {
  def apply[T](value: T,
               next: Option[DoubleLinkedNode[T]] = None,
               previous: Option[DoubleLinkedNode[T]] = None): DoubleLinkedNode[T] = {
    val newNode = new DoubleLinkedNode[T](value)
    newNode.setNext(next).setPrev(previous)
    newNode
  }
}

class DoubleLinkedNode[T](val value: T) extends Node[T] {
  var previous: Option[DoubleLinkedNode[T]] = None
  var next: Option[DoubleLinkedNode[T]] = None

  def setNext(n: Option[DoubleLinkedNode[T]]): DoubleLinkedNode[T] = {
    next = n
    this
  }

  def setPrev(p: Option[DoubleLinkedNode[T]]): DoubleLinkedNode[T] = {
    previous = p
    this
  }

  def exists(fn: T => Boolean): Boolean = {
    if (fn(value)) true
    else if (next.isEmpty) false
    else next.get.exists(fn)
  }

  def fold[A](init: A)(fn: (A, DoubleLinkedNode[T]) => A): A = {
    if (next.isEmpty) fn(init, this)
    else next.get.fold(fn(init, this))(fn)
  }

  def map[A](fn: (T) => A): (Option[DoubleLinkedNode[A]], Option[DoubleLinkedNode[A]]) = {
    if (next.isEmpty) (Some(new DoubleLinkedNode(fn(value), None, None)), Some(new DoubleLinkedNode(fn(value), None, None)))
    else {
      val (nextMappedNode, currentTail) = next.get.map(fn)
      (Some(new DoubleLinkedNode(fn(value), nextMappedNode)), currentTail)
    }
  }

  private def reverseLink(previous: Option[DoubleLinkedNode[T]]): DoubleLinkedNode[T] = {
    val nextNode = Some(DoubleLinkedNode(value, previous))
    if (next.isEmpty) nextNode.get
    else next.get.reverseLink(nextNode)
  }

  def reverse(): DoubleLinkedNode[T] = {
    if (next.isEmpty) this
    else {
      reverseLink(None)
    }
  }
}


object DoubleLinkedList {
  def apply[T](): DoubleLinkedList[T] = new DoubleLinkedList[T](None, None)
  def apply[T](node: DoubleLinkedNode[T]): DoubleLinkedList[T] = new DoubleLinkedList(Some(node), Some(node))
  def apply[T](value: T): DoubleLinkedList[T] = apply(DoubleLinkedNode(value, None, None))
}

class DoubleLinkedList[T](val head: Option[DoubleLinkedNode[T]], val tail: Option[DoubleLinkedNode[T]]) extends LinkedList[T, DoubleLinkedNode[T], DoubleLinkedList[T]] {

  def copy(newHead: Option[DoubleLinkedNode[T]] = head,
           newTail: Option[DoubleLinkedNode[T]] = tail): DoubleLinkedList[T] =
    new DoubleLinkedList[T](newHead, newTail)

  def add(node: DoubleLinkedNode[T]): DoubleLinkedList[T] = {
    val newNode = Some(node.copy(next = head))
    if (isEmpty) {
      val newNode = Some(node)
      copy(newNode, newNode)
    }
    else {

      copy(newNode, tail)
    }
  }

  def add(value: T): DoubleLinkedList[T] = add(DoubleLinkedNode(value, None, None))

  def isEmpty: Boolean = head.isEmpty

  def contains(value: T): Boolean = {
    if (head.isEmpty) false
    else head.exists(_.value == value)
  }

  def remove: DoubleLinkedList[T] = {
    if (isEmpty) throw new ArrayStoreException("LinkedList is empty, nothing to remove")
    if (head == tail) new DoubleLinkedList(None, None)
    copy(newHead = head.get.next, tail)
  }

  def reverse: DoubleLinkedList[T] = {
    if (isEmpty || (head == tail)) this
    else new DoubleLinkedList(Some(head.get.reverse()), head.map(_.copy(next = None)))
  }

  lazy val length: Int = {
    if (isEmpty) 0
    else {
      head.get.fold[Int](0){ case (agg, _) => agg + 1}
    }
  }

  override def toString: String = {
    if (isEmpty) "()"
    else if (head == tail) s"(${head.get.value})"
    else {
      val strElems = head.get.fold("") { case (str, node) =>
        if (node.next.isEmpty) str ++ node.value.toString
        else str ++ s"${node.value.toString}, "
      }
      s"($strElems)"
    }
  }

  def zip(other: DoubleLinkedList[T]): DoubleLinkedList[(T, T)] = {
    if (length != other.length) throw new IllegalArgumentException("Lengths must be equal to zip")
    else if (isEmpty) DoubleLinkedList[(T, T)]()
    else {
      head.get.fold((DoubleLinkedList[(T, T)](), other.head)) { case ((l, otherNode), node) =>
        (l.add((node.value, otherNode.get.value)), otherNode.flatMap(_.next))
      }._1.reverse
    }
  }

  def exists(fn: (T) => Boolean) = {
    if (head.isEmpty) false
    else head.get.exists(fn)
  }

  def ==(other: DoubleLinkedList[T]): Boolean = {
    if (length != other.length) false
    else if (isEmpty) true
    else {
      !this.zip(other).exists { case (x, y) => x != y }
    }
  }

  def map[A](fn: (T) => A): DoubleLinkedList[A] = {
    if (isEmpty) DoubleLinkedList[A]()
    else {
      val (newHead, newTail) = head.get.map(fn)
      new DoubleLinkedList[A](newHead, newTail)
    }
  }
}



