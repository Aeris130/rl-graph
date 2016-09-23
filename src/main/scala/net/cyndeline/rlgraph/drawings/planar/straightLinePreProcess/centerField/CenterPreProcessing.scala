package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.centerField

import net.cyndeline.rlcommon.math.geom.{Centroid, Line, Point, RPoint}
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.common.VertexAdjustment
import net.cyndeline.rlgraph.util.GraphCommons

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

/**
  * Pre-processes a planar straight-line graph by moving every vertex to the center of gravity of all its neighbors.
  * Doing this leads to more uniform edge length in drawings with areas of high vertex-density (such as planar grid
  * drawings). Since moving a vertex may introduce additional edge crossings (breaking planarity), any move that
  * induces crossings is discarded. Instead, the vertex is moved again, this time 1/7th less of the distance between its
  * original position and that of the center of gravity (thus every vertex is moved at most 7 times before the algorithm
  * gives up and proceeds to the next vertex.
  *
  * This pre-processing does not in itself produce visually pleasing layouts, but is instead used to cut down on the
  * number of iterations needed in the annealing process to deal with edge lengths. This is due to the fact that
  * planar grid drawings have high variance in edge length, depending on how far from v1/v2 the edge is drawn.
  *
  * The algorithm is iterative and can be run in a specified number of steps, as moving a vertex may result in
  * its neighbors having their center of gravity adjusted as well. Because of this there's no guarantee that the
  * coordinate set will reach a non-modifiable state, it is up to the user to decide how many times the algorithm
  * should be run.
  *
  * @param vertices Every vertex in the graph.
  * @param graph Graph used to determine the neighboring edges of each vertex.
  * @param adjustmentFactory Stores the current edges as segments and validates their movements.
  */
class CenterPreProcessing(vertices: Vector[Int],
                          graph: Graph[Int, UnDiEdge],
                          adjustmentFactory: VertexAdjustment) {

  /**
    * @param v A vertex being processed.
    * @return The current coordinate of the vertex v.
    */
  def coordinateFor(v: Int): Point = adjustmentFactory.coordinate(v)

  /** @return Every current coordinate in the processing. */
  def allCoordinates: Map[Int, Point] = adjustmentFactory.coordinate

  /**
    * Processes every vertex in the processing object once.
    * @return A pre-processing with the coordinates resulting from the adjustment, or None if no vertex could be
    *         adjusted.
    */
  def adjustAll: Option[CenterPreProcessing] = {
    var current = this
    var modified = false
    for (v <- vertices) {
      val mod = current.adjustVertex(v)
      if (mod.isDefined) {
        modified = true
        current = mod.get
      }
    }

    if (modified)
      Some(current)
    else
      None
  }

  /**
    * Adjusts a specific vertex in the processing by moving it to its neighbor-induced centroid.
    * @param v Vertex to adjust.
    * @return The processing that results from adjusting the vertex, or None if the vertex could not be adjusted
    *         without inducing additional overlap in the graph.
    */
  def adjustVertex(v: Int): Option[CenterPreProcessing] = {
    val neighbors = GraphCommons.outerNeighbors(v, graph)
    if (neighbors.isEmpty)
      return None

    /**
      * @param lineToMoveALong A line beginning in the target coordinate of v, and ending in v's current coordinate.
      * @param singleNeighbor Defined if the vertex being moved only has a single neighbor, at this point. If so, v
      *                       should be moved as close to the point as possible, but not onto it.
      */
    def processPointCandidates(lineToMoveALong: Line, singleNeighbor: Option[Point]): Option[CenterPreProcessing] = {

     /* All point candidates to try moving v to, beginning at the most favourable point (the centroid). The last
      * point is the old coordinate, so it is dropped. There will always be at least one candidate among the splits,
      * since the vertex v will never be permitted to overlap its neighbors. Note that the splits are floating point
      * coordinates cast to ints. This may result in testing multiple lines that aren't perfectly collinear with
      * each other, but that property isn't really an intrinsic part of the algorithms correctness (if the difference
      * between points are small, multiple duplicate splits will occur; These are filtered out before the algorithm
      * begins).
      */
      val vCoordinate = adjustmentFactory.coordinate(v)
      val pointCandidates = lineToMoveALong.split(7).distinct.filterNot(_ == vCoordinate).iterator

      while (pointCandidates.hasNext) {
        val nextPoint: RPoint = pointCandidates.next()
        val withNewPos = adjustmentFactory.moveVertex(v, Point(nextPoint))

        if (withNewPos.isDefined)
          return Some(new CenterPreProcessing(vertices, graph, withNewPos.get)) // Success!
      }

      None
    }

    /* If only a single neighbor exists, no centroid can be computed. Instead the vertex v is moved from its current
     * position as close as possible to its neighbor by setting the neighbors coordinate as the target. This target will
     * not be valid, but a valid adjustment may be found near it.
     */
    if (neighbors.size < 2) {
      val nCoord = adjustmentFactory.coordinate(neighbors.head)
      processPointCandidates(Line(nCoord, adjustmentFactory.coordinate(v)), Some(nCoord))
    } else {
      val neighborPoints = neighbors.map(adjustmentFactory.coordinate)
      val oldPoint: Point = adjustmentFactory.coordinate(v)
      val centroid = Centroid.fromPoints(neighborPoints.map(RPoint(_)))
      processPointCandidates(Line(centroid, RPoint(oldPoint)), None)
    }
  }

}

object CenterPreProcessing {

  /**
    * @param drawing A graph drawing.
    * @return A pre-processing factory using the vertices and coordinates in the drawing.
    */
  def apply(drawing: StraightLineDrawing[Int]): CenterPreProcessing = {
    require(!drawing.coordinates.values.exists(p => p.x < 0 || p.y < 0), "Coordinates must be >= 0.")
    val graph = Graph.from(drawing.vertices, drawing.edges.map(e => e._1~e._2))

    /* Since vertices will only be moved inwards from their neighbors, the border doesn't need margins
     * outside the outermost vertex. +1 since the end is exclusive.
     */
    val borderLength = Math.max(drawing.width, drawing.height) + 1
    val adjustments = VertexAdjustment(drawing, graph, borderLength)
    new CenterPreProcessing(drawing.vertices, graph, adjustments)
  }

}