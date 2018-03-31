package linkedlist

case class SingleLinkedNode[T](value: T, next: Option[SingleLinkedNode[T]]) {
  def exists(fn: T => Boolean): Boolean = {
    if (fn(value)) true
    else if (next.isEmpty) false
    else next.get.exists(fn)
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
  def apply[T](node: SingleLinkedNode[T]) = new SingleLinkedList(Some(node), Some(node))
}

class SingleLinkedList[T](val head: Option[SingleLinkedNode[T]], val tail: Option[SingleLinkedNode[T]]) extends LinkedList[T] {

  def copy(newHead: Option[SingleLinkedNode[T]] = head,
           newTail: Option[SingleLinkedNode[T]] = tail): SingleLinkedList[T] =
    new SingleLinkedList[T](newHead, newTail)

  def add(node: SingleLinkedNode[T]): SingleLinkedList[T] = {
    copy(Some(node.copy(next = head)))
  }

  def isEmpty: Boolean = head.isEmpty

  def contains(value: T): Boolean = {
    if (head.isEmpty) false
    else head.exists(_.value == value)
  }

  def remove: SingleLinkedList[T] = {
    if (isEmpty) throw new ArrayStoreException("LinkedList is empty, nothing to remove")
    if (head == tail) new SingleLinkedList(None, None)
    copy(newHead = head.get.next)
  }

  def reverse: SingleLinkedList[T] = {
    if (isEmpty || (head == tail)) this
    else {
      var current = head.get
      var newList = new SingleLinkedList[T](None, None)
      newList.add(current)

      while (current.next.isDefined) {
        newList.add(current.next.get)
        current = current.next.get
      }
      newList
    }
  }

  override def toString: String = {
    if (isEmpty) "()"
    if (head == tail) s"(${head.get.value})"
    else "TODO"
  }
}


