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
}
