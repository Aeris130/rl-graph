package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.common

import net.cyndeline.rlcommon.math.geom.{Line, Point}

/**
  * Given a vertex v and a set of neighbor coordinates, this class checks if any pair n1, n2 of neighbors form
  * a collinear triplet with v such that the edges of n1 and n2 overlap. This occurs when both n1 and n2 lie on the
  * same "side" of the collinear line with v.
  *
  * This is used to check for overlaps between neighboring edges when moving a vertex, a process that otherwise
  * would take O(n2) time, since every neighboring edge already intersect at v as well, making the use of
  * algorithms like Shamos Hoey impossible.
  *
  * Runtime is bounded from above by O(n log n), where n is the number of neighbors.
  */
object CollinearOverlap {

  def hasOverlap(v: Point, neighbors: Vector[(Int, Point)]): Boolean = {

    // Step 1: Compute the slope between all neighbors and v (O(n)), vertical segments give slope None
    val slopes = neighbors.map(n => {
      val point = n._2
      val l = Line(v, point)
      if (v.x != point.x)
        (Some(l.slope), point)
      else
        (None, point)
    })

    /* Step 2: Sort the segments by their slopes, causing all entries with the same slopes to end up next to
     * each other (n log n).
     */
    val sorted = slopes.sortWith((left, right) => {
      val leftSlope = left._1
      val rightSlope = right._1
      if ((leftSlope.isEmpty && rightSlope.isEmpty) || rightSlope.isEmpty)
        false
      else if (leftSlope.isEmpty)
        true
      else { // Both slopes defined
        leftSlope.get < rightSlope.get
      }
    })

    /* Step 3: Iterate over all sorted neighbors and look for collinear groups having the same slope. There are three
     * cases to consider:
     *
     * 1: A single entry with a given slope. No collinearity can exist, move on.
     * 2: A pair of collinear segments with the same slope. Check if they appear on opposite sides of v. If not, they
     *    have overlapping edges -> Exit.
     * 3: Three or more segments with the same slope. Exit without checking anything further, as 3+ points means
     *    that at least two of them must lie on the same side of v, resulting in overlap.
     *
     * O(n)
     */
    var i = 0
    val s = sorted.size

    def slope(i: Int, v: Vector[(Option[Double], Point)]): Option[Double] = v(i)._1

    def isSingle(i: Int): Boolean = if (sorted.isDefinedAt(i + 1)) {
      slope(i, sorted) != slope(i + 1, sorted)
    } else {
      true
    }

    def isTriplet(i: Int): Boolean = if (sorted.isDefinedAt(i + 1) && sorted.isDefinedAt(i + 2)) {
      slope(i, sorted) == slope(i + 1, sorted) && slope(i, sorted) == slope(i + 2, sorted)
    } else {
      false
    }

    while (i < s) {
      if (!isSingle(i)) {
        if (isTriplet(i)) {
          return true

        } else { // Has to be a pair of it gets here
          implicit val ord = Point.coordinateOrdering
          val n1P = sorted(i)._2
          val n2P = sorted(i + 1)._2
          if ((ord.lt(n1P, v) && ord.lt(n2P, v)) || (ord.gt(n1P, v) && ord.gt(n2P, v)))
            return true
          else
            i += 2
        }

      } else {
        i += 1
      }
    }

    false
  }

}
