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
  }
}
