package net.cyndeline.rlgraph.planarGraphDrawing.rectangular.dataStructure

/**
 * A single rectangle in a rectangular dual layout.
 *
 * Some rectangles are marked as dummies. These are used to fill out any holes left on the outer boundary of the final
 * rectangular dual, if they exists. Such holes are caused by gates belonging to a vertex of the graph, and one of the
 * temporary four outer vertices (North, South, West, East) needed to run the algorithm. Since those four aren't a part
 * of the final layout, neither are the gates they connect with (if any).
 *
 * @param startX Starting x coordinate of the rectangle (inclusive).
 * @param startY Starting y coordinate of the rectangle (inclusive).
 * @param stopX Stop x coordinate of the rectangle (inclusive).
 * @param stopY Stop y coordinate of the rectangle (inclusive).
 * @param isDummy True if this rectangle is used to fill the space of an absent gate on the outer boundary, false if
 *              it is an area representing a vertex in the initial graph.
 */
class Rectangle(val startX: Int, val startY: Int, val stopX: Int, val stopY: Int, val isDummy: Boolean) {
  override val toString: String = "Start(" + startX + ", " + startY + ") Stop(" + stopX + ", " + stopY + ")"

  def isGate: Boolean = false
  def isVertex: Boolean = !isDummy

  override def equals(other: Any): Boolean = other match {
    case r: Rectangle => r.startX == startX && r.startY == startY && r.stopX == stopX && r.stopY == stopY && r.isDummy == isDummy
    case _ => false
  }

  override val hashCode: Int = startX ^ startY ^ stopX ^ stopY ^ isDummy.##
}

object Rectangle {

  /** Create a regular rectangle. */
  def apply(startX: Int, startY: Int, stopX: Int, stopY: Int) = new Rectangle(startX, startY, stopX, stopY, false)

  /** Create a dummy rectangle. */
  def dummy(startX: Int, startY: Int, stopX: Int, stopY: Int) = new Rectangle(startX, startY, stopX, stopY, true)

}
