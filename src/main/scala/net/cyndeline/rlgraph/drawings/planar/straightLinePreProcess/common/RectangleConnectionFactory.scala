package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.common

import net.cyndeline.rlcommon.math.geom.{Line, Point, Rectangle}

import scala.collection.mutable.ArrayBuffer

/**
  * When a grid drawing contains edges between rectangles with sides greater than 1, it's not possible to draw straight
  * lines from the center of each rectangle without the possibility of inducing intersections with other rectangles
  * in the drawing. Instead the line must be drawn from the center of one of the sides of each rectangle.
  *
  * Example: Tw 3x3 rectangles centered at (1,1) and (5,5) may connect with four different edges: (2,1) to (5,4), (2,1)
  * to (4,5), (1,2) to (5,4) and (1,2) to (4,5).
  *
  * This factory produces every possible connection between the rectangles. Note that if both rectangles have
  * size 1, a single line will go from the center.
  *
  */
object RectangleConnectionFactory {

  /**
    * @param a A rectangle.
    * @param b A rectangle disjoint from 'a.
    * @return All lines having start coordinate at the border of 'a, and stop coordinate at 'b.
    */
  def edges(a: Rectangle, b: Rectangle): Vector[Line] = {
    val result = new ArrayBuffer[Line]()

    val aLeft = left(a)
    val aRight = right(a)
    val aTop = top(a)
    val aBot = bottom(a)

    val bLeft = left(b)
    val bRight = right(b)
    val bTop = top(b)
    val bBot = bottom(b)

    val aRightOfB = aLeft.x > bRight.x
    val aLeftOfB = aRight.x < bLeft.x
    val aAboveB = aBot.x > bTop.x
    val aBelowB = aTop.x < bBot.x

    // Regular sides (top-bottom, left-right)
    if (aRightOfB) {
      result += Line(aLeft, bRight)

      if (aBot.y > bLeft.y)
        result += Line(aBot, bLeft)
      else if (aTop.y < bLeft.y)
        result += Line(aTop, bLeft)
    }

    if (aBelowB) {
      result += Line(aTop, bBot)

      if (aRight.x < bBot.x)
        result += Line(aRight, bBot)
      else if (aLeft.x > bBot.x)
        result += Line(aLeft, bBot)
    }

    if (aLeftOfB) {
      result += Line(aRight, bLeft)

      if (aLeft.y < bBot.y)
        result += Line(aLeft, bBot)
      else if (aLeft.y > bTop.y)
        result += Line(aLeft, bTop)
    }

    if (aAboveB) {
      result += Line(aBot, bTop)

      if (aBot.x < bLeft.x)
        result += Line(aBot, bLeft)
      else if (aBot.x > bRight.x)
        result += Line(aBot, bRight)
    }

    result.toVector.distinct // Single-coordinate rectangles can create duplicates
  }

  // Computes center coordinates for each side

  private def right(r: Rectangle) = Point(r.stop.x, r.start.y + Math.floor(r.height / 2d).toInt)
  private def left(r: Rectangle) = r.start + (0, Math.floor(r.height / 2d).toInt)
  private def top(r: Rectangle) = Point(r.stop.x + Math.floor(r.width / 2d).toInt, r.stop.y)
  private def bottom(r: Rectangle) = r.start + (Math.floor(r.width / 2d).toInt, 0)

}
