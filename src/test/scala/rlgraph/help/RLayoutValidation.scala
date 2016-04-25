package rlgraph.help

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.Intersection
import net.cyndeline.rlgraph.drawings.planar.rectangular.RectangularLayout
import net.cyndeline.rlgraph.drawings.planar.rectangular.dataStructure.Rectangle
import net.cyndeline.rlgraph.util.GraphCommons
import org.scalatest.FunSpec

import scalax.collection.GraphEdge.UnDiEdge

/**
 * Validates rectangular layouts.
 */
object RLayoutValidation {
  def layoutIsValid[V](layout: RectangularLayout[V, UnDiEdge]) {
    new RLayoutValidation[V]().layoutIsValid(layout, false)
  }

  /**
   * Used when the layout has been processed by a heuristic that ignores some adjacencies.
   */
  def layoutIsValidIgnoreCorners[V](layout: RectangularLayout[V, UnDiEdge]) {
    new RLayoutValidation[V]().layoutIsValid(layout, true)
  }

  def gatesAreAdjacentToNeighbors[V](layout: RectangularLayout[V, UnDiEdge]) {
    new RLayoutValidation[V]().gatesAreAdjacentToNeighbors(layout)
  }
}

class RLayoutValidation[V] private () extends FunSpec {

  def layoutIsValid(drawing: RectangularLayout[V, UnDiEdge], ignoreCorners: Boolean) {
    noOverlappingRectangles(drawing)
    coordinatesUsed(drawing)

    if (!ignoreCorners)
      noTripleIntersectingCorners(drawing)

    gatesAreAdjacentToNeighbors(drawing)
    areasAdjacentToNeighbors(drawing)
  }

  def gatesAreAdjacentToNeighbors(drawing: RectangularLayout[V, UnDiEdge]): Unit = {
    for (g <- drawing.gates) {
      val n1 = drawing.rectangles(g.from)
      val n2 = drawing.rectangles(g.to)
      val intersectionN1 = Intersection(Point(g.startX, g.startY), Point(g.stopX, g.stopY), Point(n1.startX, n1.startY), Point(n1.stopX, n1.stopY))
      val intersectionN2 = Intersection(Point(g.startX, g.startY), Point(g.stopX, g.stopY), Point(n2.startX, n2.startY), Point(n2.stopX, n2.stopY))

      if (!intersectionN1.intersects)
        fail("The neighbor " + g.from + " did not intersect with its gate " + g)
      else if (!intersectionN2.intersects)
        fail("The neighbor " + g.to + " did not intersect with its gate " + g)
    }
  }

  private def noOverlappingRectangles(drawing: RectangularLayout[V, UnDiEdge]): Unit = {
    for (r <- drawing.rectangles.values ++ drawing.gates) {
      val overlap = drawing.rectangles.values.find(other => other != r && intersects(r, other))
      if (overlap.isDefined) {
        fail("Overlapping rectangles in drawing: " + r + ", " + overlap.get)
      }
    }
  }

  private def coordinatesUsed(drawing: RectangularLayout[V, UnDiEdge]): Unit = {
    for (x <- drawing.minX to drawing.maxX; y <- drawing.minY to drawing.maxY) {
      if (!(drawing.rectangles.values ++ drawing.gates ++ drawing.dummies).exists(r => x >= r.startX && x <= r.stopX && y >= r.startY && y <= r.stopY)) {
        fail("The coordinate (" + x + ", " + y + ") is inside the min/max y coordinates for " + drawing + ", but not covered by a rectangle.")
      }
    }
  }

  private def noTripleIntersectingCorners(drawing: RectangularLayout[V, UnDiEdge]): Unit = {
    for (r <- drawing.rectangles.values ++ drawing.gates) {
      val rCorners = corners(r)
      val intersecting1 = drawing.rectangles.values.find(other => {
        val otherCorners = corners(other)
        other != r && (otherCorners intersect rCorners).nonEmpty
      })
      val intersecting2 = if (intersecting1.isDefined) {
        drawing.rectangles.values.find(other => {
          val otherCorners = corners(other)
          other != r && other != intersecting1.get && (otherCorners intersect rCorners).nonEmpty
        })
      } else {
        None
      }
      if (intersecting1.isDefined && intersecting2.isDefined) {
        val rCorners = corners(r)
        val i1Corners = corners(intersecting1.get)
        val i2Corners = corners(intersecting2.get)

        if (rCorners.intersect(i1Corners.intersect(i2Corners)).nonEmpty)
          fail("Intersecting corners between " + r + ", " + intersecting1.get + " and " + intersecting2.get)
      }
    }
  }

  private def areasAdjacentToNeighbors(drawing: RectangularLayout[V, UnDiEdge]): Unit = {
    for (n <- drawing.graph.nodes) {
      val area = drawing.rectangles(n)
      val neighbors = GraphCommons.outerNeighbors[V, UnDiEdge](n, drawing.graph)

      for (neighbor <- neighbors) {
        val neighborArea = drawing.rectangles(neighbor)

        val intersection = Intersection(Point(area.startX, area.startY), Point(area.stopX, area.stopY), Point(neighborArea.startX, neighborArea.startY), Point(neighborArea.stopX, neighborArea.stopY))
        if (!intersection.intersects && !sharesGate(n, neighbor, drawing)) {
          fail("The vertices " + n + " and " + neighbor + " did not share a gate, and wasn't adjacent in the final drawing.")
        }
      }
    }

  }

  private def intersects(a: Rectangle, b: Rectangle): Boolean = {
    !(a.stopX <= b.startX || b.startX <= a.stopX || a.stopY <= b.startY || b.stopY <= a.startY)
  }

  private def corners(r: Rectangle): Set[(Int, Int)] = Set((r.startX, r.startY), (r.stopX, r.stopY), (r.startX, r.stopY), (r.stopX, r.startY))

  private def sharesGate(a: V, b: V, layout: RectangularLayout[V, UnDiEdge]): Boolean = layout.gates.exists(g => Set(g.from, g.to) == Set(a, b))

}
