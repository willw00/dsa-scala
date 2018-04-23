import scala.collection.mutable.ArrayBuffer
import util.Random

package object sorting {
  implicit class BubbleSorter(a: Array[Int]) {
    def bubbleSort: Int = {
      val length = a.length
      var comparisons = 0

      (0 until length) foreach { i =>
        (0 until length) foreach { j =>
          val iVal = a(i)
          val jVal = a(j)
          comparisons += 1
          if (iVal < jVal) {
            a(j) = iVal
            a(i) = jVal
          }
        }
      }

      comparisons
    }
  }

  implicit class MergeSorter(a: Array[Int]) {
    def merge(x: Array[Int]): Array[Int] = {
      val z = Array.fill(a.length + x.length)(0)
      var xCounter = 0
      var aCounter = 0
      var zCounter = 0

      while (xCounter < x.length && aCounter < a.length) {
        if (x(xCounter) < a(aCounter)) {
          z(zCounter) = x(xCounter)
          xCounter += 1
        }
        else {
          z(zCounter) = a(aCounter)
          aCounter += 1
        }
        zCounter += 1
      }

      while (xCounter < x.length) {
        z(zCounter) = x(xCounter)
        xCounter += 1
        zCounter += 1
      }

      while (aCounter < a.length) {
        z(zCounter) = a(aCounter)
        aCounter += 1
        zCounter += 1
      }

      z
    }


    def mergeSort: Array[Int] = {
      val length = a.length

      if (length <= 1) a
      else {
        val m = length / 2
        val left = a.slice(0, m).mergeSort
        val right = a.slice(m, length).mergeSort

        val merged = left.merge(right.mergeSort)
        merged
      }
    }
  }

  implicit class QuickSorter(a: Array[Int]) {
    def quickSort: Array[Int] = {
      if (a.length <= 1) return a

      val pivot = Random.nextInt(a.length)
      val pivotVal = a(pivot)
      val lessThan = ArrayBuffer[Int]()
      val equalTo = ArrayBuffer[Int]()
      val greaterThan = ArrayBuffer[Int]()
      a.foreach { e =>
        if (e < pivotVal) lessThan += e
        else if (e == pivotVal) equalTo += e
        else greaterThan += e
      }

      lessThan.toArray.quickSort ++ equalTo ++ greaterThan.toArray.quickSort
    }
  }
}
