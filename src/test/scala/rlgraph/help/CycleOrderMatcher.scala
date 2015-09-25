package rlgraph.help

/**
 * Compares the contents of a list when its elements are in unknown cyclical order.
 * Example: Comparing the list [1, 2, 3] with another list [3, 1, 2] yields true.
 */
class CycleOrderMatcher {

  /**
   * @param unknownOrder List of elements whose cyclical order is unknown.
   * @param expected A list of cyclical order specified by the user.
   * @return True if the elements in 'unknown can be rotated to match the expected list, otherwise false.
   */
  def compares[E](unknownOrder: Vector[E], expected: Vector[E]): Boolean = {
    if (expected.isEmpty) throw new IllegalArgumentException("Must expect at least one vertex")
    var n = 0
    var expect: Vector[E] = expected
    while(unknownOrder != expect && n <= unknownOrder.size) {
      val tmp = expect(0)
      expect = expect drop 1
      expect = expect :+ tmp
      n += 1
    }

    unknownOrder == expect
  }

  /**
   * Checks if an unknown list compares to a certain order in either direction.
   * Example: The list [4, 3, 2, 1] matches the expected order [1, 2, 3, 4] since the list is a reverse.
   *
   * @param unknownOrder List of elements whose cyclical order is unknown.
   * @param expected A list of cyclical order specified by the user.
   * @return True if the unknown list matches the cyclical order of the expected list in either direction, otherwise false.
   */
  def compareBothDirections(unknownOrder: Vector[Int], expected: Vector[Int]): Boolean = {
    compares(unknownOrder, expected) || compares(unknownOrder, expected.reverse)
  }

}
