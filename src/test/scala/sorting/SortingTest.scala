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
  }
}
