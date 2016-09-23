package net.cyndeline.rlgraph.drawings.forceDirected.fruchtermanReingold.grid

import net.cyndeline.rlcommon.math.ClosestDivisor
import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlgraph.drawings.forceDirected.fruchtermanReingold.grid.VertexGrid.RectangleStorage

/**
  * Used to decrease the runtime complexity of the force-directed algorithm from O(n2) to O(n) by splitting the canvas
  * into a grid where each cell represents a range of coordinates. When determining if a vertex exists that should
  * assert a repulsing force on the vertex currently being processed, only the vertices that lies within a specified
  * circumference will be taken into consideration. To find these vertices, every cell that is covered (even
  * partly) by the circumference will be used when retrieving vertices.
  *
  * The usage of this grid diverts from its original concept in that it doesn't store which vertices reside in a
  * particular cell, but rather which rectangle (representing a vertex) overlaps the cell. Multiple cells can thus
  * store a rectangle belonging to a single vertex. When checking for vertex presence using the circumference,
  * rectangle overlap with the circumference is that is taken into consideration.
  *
  * Start coordinates are inclusive, stop coordinate are exclusive.
  *
  * @param vertices Every vertex id mapped against its rectangle.
  */
class VertexGrid(cellWidth: Int, cellHeight: Int, grid: Vector[Vector[RectangleStorage]], val vertices: Map[Int, GridRectangle]) {
  require(grid.nonEmpty, "Cannot instantiate an empty vertex grid.")

  val width = grid.length * cellWidth
  val height = grid(0).length * cellHeight

  /**
    * @return True if no rectangles are stored in the grid, otherwise false.
    */
  def isEmpty: Boolean = {
    if (vertices.nonEmpty) {
      false

    } else {
      for (col <- grid; row <- col if !row.isEmpty)
        return false

      true
    }
  }

  /**
    * Creates a new rectangle.
    * @param vertexId Unique identifier of the vertex represented by the rectangle.
    * @param upperLeftCoordinate Upper left rectangle coordinate on the grid.
    * @param width Total width of the rectangle. Must be odd.
    * @param height Total height of the rectangle. Must be odd.
    * @return The new rectangle, and the updated grid containing it.
    */
  def create(vertexId: Int, upperLeftCoordinate: Point, width: Int, height: Int): (GridRectangle, VertexGrid) = {
    require(width > 0 && height > 0, "Rectangle width and height must be greater than 0")
    require(!vertices.contains(vertexId), "A vertex with id " + vertexId + " has already been added to the grid.")
    val r = GridRectangle(vertexId, upperLeftCoordinate, width, height)

    (r, this.add(r))
  }

  /**
    * Moves a vertex to a new coordinate space in the grid.
    * @param v Vertex rectangle to move.
    * @param dx Amount to modify the rectangles x coordinate by.
    * @param dy Amount to modify the rectangles y coordinate by.
    * @return A copy of this grid with the vertex position updated.
    */
  def move(v: GridRectangle, dx: Int, dy: Int): VertexGrid = {
    remove(v).add(v.move(dx, dy, this))
  }

  /**
    * @return Every vertex rectangle in the grid.
    */
  def allVertices: Vector[GridRectangle] = allVertices(Point(0, 0), Point(width, height))

  /**
    * @param from Start coordinate to retrieve vertex rectangles from. Inclusive.
    * @param to End coordinate, exclusive.
    * @return Every vertex rectangle stored within the coordinate space.
    */
  def allVertices(from: Point, to: Point): Vector[GridRectangle] = {
    require(from.x < to.x && from.y < to.y, "The coordinate intervals stop point must be strictly less than the start.")
    require(from.x >= 0 && from.y >= 0, "Start coordinates must be 0 or greater when examining the grid.")
    require(to.x <= width && to.y <= height, "Attempted to retrieve grid rectangles outside of grid boundary.")
    val cellInt = cellInterval(from, to.x - from.x, to.y - from.y)
    val allRectangles = for (i <- cellInt._1 until cellInt._3; j <- cellInt._2 until cellInt._4) yield grid(i)(j).rectangles
    allRectangles.flatten.distinct.toVector
  }

  /**
    * Adds a new rectangle to the grid.
    * @param v Vertex rectangle to add.
    * @return The grid updated with the new rectangle.
    */
  private def add(v: GridRectangle): VertexGrid = {
    require(v.isInside(this), "Attempted to add vertex " + v + " that exceeds the grid boundaries (" + width + ", " + height + ").")
    val cells = cellInterval(v)
    modify(cells._1, cells._2, cells._3, cells._4, v, addR = true)
  }

  /**
    * Removes a new rectangle from the grid.
    * @param v Vertex rectangle to remove.
    * @return The grid updated without the new rectangle.
    */
  private def remove(v: GridRectangle): VertexGrid = {
    val cells = cellInterval(v)
    modify(cells._1, cells._2, cells._3, cells._4, v, addR = false)
  }

  private def modify(fromX: Int, fromY: Int, toX: Int, toY: Int, r: GridRectangle, addR: Boolean): VertexGrid = {
    var g = grid

    for (i <- fromX until toX; j <- fromY until toY) {
      val storage = grid(i)(j)
      g = g.updated(i, g(i).updated(j, if (addR) storage.add(r) else storage.remove(r)))
    }

    new VertexGrid(cellWidth, cellHeight, g, if (addR) vertices + (r.id -> r) else vertices - r.id)
  }

  /**
    * @param v A vertex rectangle.
    * @return Start and stop values for every cell index overlapped by that rectangle.
    */
  private def cellInterval(v: GridRectangle): (Int, Int, Int, Int) = cellInterval(Point(v.start), v.width.intValue(), v.height.intValue())


  /**
    * @param start Upper left point of a rectangular area.
    * @param width Width of the rectangle.
    * @param height Height of the rectangle.
    * @return Start x and start y (inclusive) as well as stop x and stop y (exclusive) of the cell intervals overlapped
    *         by the rectangle.
    */
  private def cellInterval(start: Point, width: Int, height: Int): (Int, Int, Int, Int) = {
    val lowestCellX = Math.floor(start.x / cellWidth.toDouble).toInt
    val lowestCellY = Math.floor(start.y / cellHeight.toDouble).toInt
    val highestCellX = Math.ceil((start.x + width) / cellWidth.toDouble).toInt
    val highestCellY = Math.ceil((start.y + height) / cellHeight.toDouble).toInt

    (lowestCellX, lowestCellY, highestCellX, highestCellY)
  }
}

/**
  * Factory object.
  */
object VertexGrid {

  /**
    * Stores rectangles in a coordinate cell.
    *
    * @param rectangles A vector containing indices for every rectangle regardless of weather or not it's stored here.
    *                   Contains null objects for rectangle id's that are not present.
    */
  private class RectangleStorage(val rectangles: Vector[GridRectangle]) {
    def remove(r: GridRectangle): RectangleStorage = {
      require(rectangles.contains(r), "Attempted to remove rectangle " + r + ", but no such rectangle was present in the grid storage.")
      new RectangleStorage(rectangles diff Vector(r))
    }
    def add(r: GridRectangle): RectangleStorage = {
      require(!rectangles.contains(r), "Attempted to add rectangle " + r + ", but a rectangle was already present in the grid storage.")
      new RectangleStorage(rectangles :+ r)
    }
    def isEmpty: Boolean = rectangles.isEmpty
  }

  /**
    * @param gridWidth Number of coordinates on the grids x axis.
    * @param gridHeight Number of coordinates on the grids y axis.
    * @param cellSize The number of coordinates that should be present in each cell. If the grid cannot be evenly split
    *                 into cells of this size, the nearest evenly divisor will be used instead. Must be equal to or less
    *                 than the grids size on each axis.
    */
  def apply(gridWidth: Int, gridHeight: Int, cellSize: Int): VertexGrid = {
    require(cellSize <= gridWidth && cellSize <= gridHeight, "The cell size in a vertex grid must be equal or less than the grids size on each axis.")
    require(cellSize >= 1, "The cell size in a vertex grid must be equal or greater than 1 (currently " + cellSize + ").")
    val emptyRectangleStorage = new RectangleStorage(Vector())
    val remainderX = gridWidth % cellSize
    val remainderY = gridHeight % cellSize

    val adjustedCellSizeX = if (remainderX > 0) {
      new ClosestDivisor().findClosestDivisor(gridWidth, cellSize)
    } else {
      cellSize
    }
    val adjustedCellSizeY = if (remainderY > 0) {
      new ClosestDivisor().findClosestDivisor(gridHeight, cellSize)
    } else {
      cellSize
    }

    val grid = Vector.fill(gridWidth / adjustedCellSizeX, gridHeight / adjustedCellSizeY){ emptyRectangleStorage }
    new VertexGrid(adjustedCellSizeX, adjustedCellSizeY, grid, Map())
  }

}
