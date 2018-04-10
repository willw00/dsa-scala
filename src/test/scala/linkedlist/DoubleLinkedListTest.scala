package linkedlist

import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}


class DoubleLinkedListTest extends WordSpec with Matchers with BeforeAndAfterEach {

  var emptyList: DoubleLinkedList[Int] = _
  var aList: DoubleLinkedList[Int] = _
  var moreList: DoubleLinkedList[Int] = _

  override def beforeEach(): Unit = {
    emptyList = DoubleLinkedList[Int]()
    aList = emptyList.add(1)
    moreList = aList.add(2).add(3).add(4).add(5)
  }

  "A DoubleLinkedList" should {

    "contain the correct head and tail" in {
      emptyList.head shouldEqual None
      emptyList.tail shouldEqual None
      aList.head.get.value shouldEqual 1
      aList.tail.get.value shouldEqual 1
      moreList.head.get.value shouldEqual 5
      moreList.tail.get.value shouldEqual 1
    }

    "properly return isEmpty" in {
      emptyList.isEmpty shouldEqual true
      aList.isEmpty shouldEqual false
    }

    "properly print itself" in {
      emptyList.toString shouldEqual "()"
      aList.toString shouldEqual "(1)"
      moreList.toString shouldEqual "(5, 4, 3, 2, 1)"
    }

    "properly compute its length" in {
      emptyList.length shouldEqual 0
      aList.length shouldEqual 1
      moreList.length shouldEqual 5
    }

    "properly reverse itself" in {
      emptyList.reverse.toString shouldEqual "()"
      aList.reverse.toString shouldEqual "(1)"
      moreList.reverse.toString shouldEqual "(1, 2, 3, 4, 5)"
    }

    "properly map itself" in {
      emptyList.map( _ + 1).toString shouldEqual "()"
      aList.map( _ + 1).toString shouldEqual "(2)"
      moreList.map( _ + 1).toString shouldEqual "(6, 5, 4, 3, 2)"
    }

    "properly zip two lists" in {
      emptyList.zip(emptyList).toString shouldEqual "()"
      aList.zip(aList).toString shouldEqual "((1,1))"
      moreList.zip(moreList).toString shouldEqual "((5,5), (4,4), (3,3), (2,2), (1,1))"
      val other = DoubleLinkedList[Int]().add(5).add(4).add(3).add(2).add(1)
      moreList.zip(other).toString shouldEqual "((5,1), (4,2), (3,3), (2,4), (1,5))"
    }

    "properly evaluate equality" in {
      (emptyList == emptyList) shouldEqual true
      (emptyList == aList) shouldEqual false
      (aList == aList) shouldEqual true
      (aList == moreList) shouldEqual false
      (moreList == moreList) shouldEqual true

      val other = DoubleLinkedList[Int]().add(5).add(4).add(3).add(2).add(1)
      val other2 = DoubleLinkedList[Int]().add(1)

      (other == moreList) shouldEqual false
      (emptyList.reverse == emptyList) shouldEqual true
      (other2 == aList) shouldEqual true
    }

    "properly evaluate contains" in {
      emptyList.contains(3) shouldEqual false
      aList.contains(1) shouldEqual true
      aList.contains(9) shouldEqual false
      moreList.contains(4) shouldEqual true
      moreList.contains(-1) shouldEqual false
    }
  }
}

