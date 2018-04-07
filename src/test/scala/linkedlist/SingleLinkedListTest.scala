package linkedlist

import org.scalatest.{Matchers, WordSpec}


class SingleLinkedListTest extends WordSpec with Matchers {
  "A LinkedList" should {
    val empty = SingleLinkedList[Int]()
    val aList = empty.add(1)
    val moreList = aList.add(2).add(3).add(4).add(5)

    "contain the correct head and tail" in {
      empty.head shouldEqual None
      empty.tail shouldEqual None
      aList.head.get.value shouldEqual 1
      aList.tail.get.value shouldEqual 1
      moreList.head.get.value shouldEqual 5
      moreList.tail.get.value shouldEqual 1
    }

    "properly return isEmpty" in {
      empty.isEmpty shouldEqual true
      aList.isEmpty shouldEqual false
    }

    "properly print itself" in {
      empty.toString shouldEqual "()"
      aList.toString shouldEqual "(1)"
      moreList.toString shouldEqual "(5, 4, 3, 2, 1)"
    }

    "properly compute its length" in {
      empty.length shouldEqual 0
      aList.length shouldEqual 1
      moreList.length shouldEqual 5
    }

    "properly reverse itself" in {
      empty.reverse.toString shouldEqual "()"
      aList.reverse.toString shouldEqual "(1)"
      moreList.reverse.toString shouldEqual "(1, 2, 3, 4, 5)"
    }

    "properly map itself" in {
      empty.map( _ + 1).toString shouldEqual "()"
      aList.map( _ + 1).toString shouldEqual "(2)"
      moreList.map( _ + 1).toString shouldEqual "(6, 5, 4, 3, 2)"
    }

    "properly zip two lists" in {
      empty.zip(empty).toString shouldEqual "()"
      aList.zip(aList).toString shouldEqual "((1,1))"
      moreList.zip(moreList).toString shouldEqual "((5,5), (4,4), (3,3), (2,2), (1,1))"
      moreList.zip(moreList.reverse).toString shouldEqual "((5,1), (4,2), (3,3), (2,4), (1,5))"
    }

    "properly evaluate equality" in {
      (empty == empty) shouldEqual true
      (empty == aList) shouldEqual false
      (aList == aList) shouldEqual true
      (aList == moreList) shouldEqual false
      (moreList == moreList) shouldEqual true
      (moreList.reverse == moreList) shouldEqual false
      (empty.reverse == empty) shouldEqual true
      (aList.reverse == aList) shouldEqual true
    }
  }
}
