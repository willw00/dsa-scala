package linkedlist

case class SingleLinkedNode[T](value: T, next: Option[SingleLinkedNode[T]]) {
  def exists(fn: T => Boolean): Boolean = {
    if (fn(value)) true
    else if (next.isEmpty) false
    else next.get.exists(fn)
  }

  def fold[A](init: A)(fn: (A, SingleLinkedNode[T]) => A): A = {
    if (next.isEmpty) fn(init, this)
    else next.get.fold(fn(init, this))(fn)
  }

  def map[A](fn: (T) => A): (Option[SingleLinkedNode[A]], Option[SingleLinkedNode[A]]) = {
    if (next.isEmpty) (Some(SingleLinkedNode(fn(value), None)), Some(SingleLinkedNode(fn(value), None)))
    else {
      val (nextMappedNode, currentTail) = next.get.map(fn)
      (Some(SingleLinkedNode(fn(value), nextMappedNode)), currentTail)
    }
  }

  private def reverseLink(previous: Option[SingleLinkedNode[T]]): SingleLinkedNode[T] = {
    val nextNode = Some(SingleLinkedNode(value, previous))
    if (next.isEmpty) nextNode.get
    else next.get.reverseLink(nextNode)
  }

  def reverse(): SingleLinkedNode[T] = {
    if (next.isEmpty) this
    else {
      reverseLink(None)
    }
  }
}

trait LinkedList[T] {
  def head: Option[SingleLinkedNode[T]]
  def tail: Option[SingleLinkedNode[T]]

  //Append to front of list
  def add(node: SingleLinkedNode[T]): LinkedList[T]
  //Remove from head of list
  def remove: SingleLinkedList[T]
  def contains(value: T): Boolean

  def isEmpty: Boolean
}

object SingleLinkedList {
  def apply[T](): SingleLinkedList[T] = new SingleLinkedList[T](None, None)
  def apply[T](node: SingleLinkedNode[T]): SingleLinkedList[T] = new SingleLinkedList(Some(node), Some(node))
  def apply[T](value: T): SingleLinkedList[T] = apply(SingleLinkedNode(value, None))
}

class SingleLinkedList[T](val head: Option[SingleLinkedNode[T]], val tail: Option[SingleLinkedNode[T]]) extends LinkedList[T] {

  def copy(newHead: Option[SingleLinkedNode[T]] = head,
           newTail: Option[SingleLinkedNode[T]] = tail): SingleLinkedList[T] =
    new SingleLinkedList[T](newHead, newTail)

  def add(node: SingleLinkedNode[T]): SingleLinkedList[T] = {
    val newNode = Some(node.copy(next = head))
    if (isEmpty) {
      copy(newNode, newNode)
    }
    else {
      copy(newNode, tail)
    }
  }

  def add(value: T): SingleLinkedList[T] = add(SingleLinkedNode(value, None))

  def isEmpty: Boolean = head.isEmpty

  def contains(value: T): Boolean = {
    if (head.isEmpty) false
    else head.exists(_.value == value)
  }

  def remove: SingleLinkedList[T] = {
    if (isEmpty) throw new ArrayStoreException("LinkedList is empty, nothing to remove")
    if (head == tail) new SingleLinkedList(None, None)
    copy(newHead = head.get.next, tail)
  }

  def reverse: SingleLinkedList[T] = {
    if (isEmpty || (head == tail)) this
    else new SingleLinkedList(Some(head.get.reverse()), head.map(_.copy(next = None)))
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

//  def zip(other: SingleLinkedList[T]): SingleLinkedList[(T, T)] = {
//    if (length != other.length) throw new IllegalArgumentException("Lengths must be equal to zip")
//    else {
//
//    }
//  }

//  def ==(other: SingleLinkedList[T]): Boolean = {
//    if (length != other.length) false
//    else {
//
//    }
//  }

  def map[A](fn: (T) => A): SingleLinkedList[A] = {
    if (isEmpty) SingleLinkedList[A]()
    else {
      val (newHead, newTail) = head.get.map(fn)
      new SingleLinkedList[A](newHead, newTail)
    }
  }
}


