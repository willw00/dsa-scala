package searchtree

import org.scalatest.{Matchers, WordSpec}

class BinarySearchTreeTest extends WordSpec with Matchers {

  "A BinarySearchTree" should {
    val t = BinarySearchTree.empty

    "properly insert and retrieve values" in {
      val x = t.insert(5).insert(3).insert(1).insert(10).insert(19).insert(7)
      x.contains(1) shouldEqual true
      x.contains(2) shouldEqual false
      x.contains(19) shouldEqual true
      x.contains(7) shouldEqual true
      x.contains(4) shouldEqual false
    }

    "properly find a parent" in {
      val x = t.insert(5).insert(3).insert(1).insert(7).insert(6).insert(8)

      x.findParent(4) shouldEqual None
      x.findParent(5) shouldEqual None
      x.findParent(3).get.value shouldEqual 5
      x.findParent(1).get.value shouldEqual 3
      x.findParent(7).get.value shouldEqual 5
      x.findParent(6).get.value shouldEqual 7
      x.findParent(8).get.value shouldEqual 7
    }

    "properly count the number of elements" in {
      t.count shouldEqual 0
      t.insert(5).count shouldEqual 1
      t.insert(5).insert(3).insert(7).insert(8).count shouldEqual 4
    }

    "properly compute equality" in {
      val x = t.insert(5).insert(3).insert(1).insert(7).insert(6).insert(8)
      val y = t.insert(5).insert(3).insert(1).insert(10).insert(19).insert(7)

      t == t shouldEqual true
      t == x shouldEqual false
      x == x shouldEqual true
      x == y shouldEqual false
    }

    "properly find the largest node" in {
      val x = t.insert(5).insert(3).insert(1).insert(7).insert(6).insert(8)

      t.findLargest shouldEqual None
      x.findLargest.get shouldEqual 8
    }

    "properly find the smallest node" in {
      val x = t.insert(5).insert(3).insert(1).insert(7).insert(6).insert(8)

      t.findSmallest shouldEqual None
      x.findSmallest.get shouldEqual 1
    }

    "properly delete a value" in {
      val x = t.insert(5)
      val y = x.insert(3).insert(1).insert(7).insert(6).insert(8)

      t.delete(0) shouldEqual t
      x.delete(5) shouldEqual t
      y.delete(1) shouldEqual t.insert(5).insert(3).insert(7).insert(6).insert(8)
      y.delete(3) shouldEqual t.insert(5).insert(1).insert(7).insert(6).insert(8)
      y.delete(7) shouldEqual t.insert(5).insert(3).insert(1).insert(6).insert(8)
      y.delete(6) shouldEqual t.insert(5).insert(3).insert(1).insert(7).insert(8)
      y.delete(8) shouldEqual t.insert(5).insert(3).insert(1).insert(7).insert(6)
      y.delete(5) shouldEqual t.insert(3).insert(1).insert(7).insert(6).insert(8)
    }
  }
}
