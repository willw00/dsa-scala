package sorting

import org.scalatest.{Matchers, WordSpec}
import util.Random

class SortingTest extends WordSpec with Matchers {

  def arraysEqual(x: Array[Int], y: Array[Int]): Boolean = {
    if (x.length != y.length) false
    else if (x.length == 0) true
    else x.zip(y).forall {z => z._1 == z._2}
  }

  "An array" should {

    "be properly sortable using Bubble Sort" in {
      val emptyArray = Array.empty[Int]
      val singleElement = Array(10)
      val a = Array.fill(100)(Random.nextInt)
      val aCopy = a.clone()

      println(s"Empty Bubble comparisons: ${emptyArray.bubbleSort}")
      println(s"Single Bubble comparisons: ${singleElement.bubbleSort}")
      println(s"Large Bubble comparisons: ${a.bubbleSort}")

      arraysEqual(emptyArray, Array.empty[Int]) shouldEqual true
      arraysEqual(singleElement, Array(10)) shouldEqual true
      arraysEqual(a, aCopy.sorted) shouldEqual true
    }

    "correctly merge with another array" in {
      val emptyArray = Array.empty[Int]
      val singleElement = Array(10)
      val others = Array(1, 2, 3, 11)
      val others2 = Array(-1, 2, 5, 12)

      arraysEqual(singleElement.merge(emptyArray), Array(10)) shouldEqual true
      arraysEqual(singleElement.merge(others), Array(1, 2, 3, 10, 11)) shouldEqual true
      arraysEqual(others.merge(others2), Array(-1, 1, 2, 2, 3, 5, 11, 12)) shouldEqual true
    }

    "be properly sortable using Merge Sort" in {
      val emptyArray = Array.empty[Int]
      val singleElement = Array(10)
      val a = Array.fill(10)(Random.nextInt)
      val aCopy = a.clone()

      val emptySorted = emptyArray.mergeSort
      val singleSorted = singleElement.mergeSort
      val aSorted = a.mergeSort


      arraysEqual(emptySorted, Array.empty[Int]) shouldEqual true
      arraysEqual(singleSorted, Array(10)) shouldEqual true
      arraysEqual(aSorted, aCopy.sorted) shouldEqual true
    }

    "Be properly sortable using Quick Sort" in {
      val emptyArray = Array.empty[Int]
      val singleElement = Array(10)
      val a = Array.fill(10)(Random.nextInt)
      val aCopy = a.clone()

      val emptySorted = emptyArray.quickSort
      val singleSorted = singleElement.quickSort
      val aSorted = a.quickSort


      arraysEqual(emptySorted, Array.empty[Int]) shouldEqual true
      arraysEqual(singleSorted, Array(10)) shouldEqual true
      arraysEqual(aSorted, aCopy.sorted) shouldEqual true
    }
  }
}
