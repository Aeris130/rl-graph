package net.cyndeline.rlgraph.drawings.forceDirected.fruchtermanReingold.grid

import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}

/**
  * @param id A unique identifier belonging to the vertex represented by this rectangle.
  * @param s The left corner of the rectangle closest to (0, 0) when the rectangle has positive coordinates.
  * @param w Rectangle width.
  * @param h Rectangle height.
  */
class GridRectangle(val id: Int, val s: Point, val w: Int, val h: Int) extends Rectangle(s, w, h) {

  /** The center coordinate of the rectangle. */
  val center: Point = Point(start.x + Math.floor(width.toDouble / 2).toInt, start.y + Math.floor(height.toDouble / 2).toInt)

  /** Exclusive ending coordinate for the rectangle. */
  val stopCoordinate: Point = start + (width, height)

  /**
    * @param dx Amount to modify the rectangles x coordinate by.
    * @param dy Amount to modify the rectangles y coordinate by.
    * @param grid The grid that the rectangle is stored in.
    * @return A new copy of the rectangle with its position modified.
    */
  def move(dx: Int, dy: Int, grid: VertexGrid): GridRectangle = {
    val errorStr = "Attempted to move rectangle outside vertex grid bounds: " + GridRectangle
    val moved = GridRectangle(id, start + (dx, dy), w, h)
    require(moved.isInside(grid), errorStr)
    moved
  }

  /**
    * @param grid Grid to check boundary for.
    * @return True if the entire rectangle lies inside the grids start and stop coordinates, otherwise false.
    */
  def isInside(grid: VertexGrid): Boolean = {
    start.x >= 0 && start.y >= 0 && stopCoordinate.x <= grid.width && stopCoordinate.y <= grid.height
  }

  override def equals(other: Any): Boolean = other match {
    case gr: GridRectangle => id == gr.id && start == gr.start && width == gr.width && height == gr.height
    case _ => false
  }

  override def hashCode: Int = id.## ^ start.## ^ width.## ^ height.##

  override def toString: String = "GR[" + id + ", " + start + ", w: " + width + ", h: " + height + "]"
}

/**
  * Factory object.
  */
object GridRectangle {

  /**
    * @param id A unique identifier belonging to the vertex represented by this rectangle.
    * @param s The left corner of the rectangle closest to (0, 0) when the rectangle has positive coordinates.
    * @param w Rectangle width.
    * @param h Rectangle height.
    */
  def apply(id: Int, s: Point, w: Int, h: Int): GridRectangle = new GridRectangle(id, s, w, h)
}
