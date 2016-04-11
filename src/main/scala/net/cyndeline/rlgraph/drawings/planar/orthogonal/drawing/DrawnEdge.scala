package net.cyndeline.rlgraph.drawings.planar.orthogonal.drawing

import scala.language.higherKinds
import scalax.collection.GraphEdge.UnDiEdge

/**
 * A single edge in an orthogonal drawing.
 *
 * @constructor Creates a new edge in an orthogonal drawing.
 * @param start The vertex the edge starts at.
 * @param stop The vertex the edge stops at.
 * @param startPos Starting coordinate of the edge in the drawing.
 * @param stopPos Stop coordinate of the edge in the drawing.
 * @param bends A list of all coordinates where the edge bends, as they are visited when traversing from the start
 *              coordinate to the stop.
 * @param originalEdge The edge this drawing is based on.
 */
class DrawnEdge[VType, EType[X] <: UnDiEdge[X]](val start: VType,
                       val stop: VType,
                       val startPos: (Int, Int),
                       val stopPos: (Int, Int),
                       val bends: Vector[(Int, Int)],
                       val originalEdge: EType[VType]) {

  /**
   * Creates a new edge without bends.
 *
   * @param start The vertex the edge starts at.
   * @param stop The vertex the edge stops at.
   * @param startPos Starting coordinate of the edge in the drawing.
   * @param stopPos Stop coordinate of the edge in the drawing.
   * @param originalEdge The edge this drawing is based on.
   * @return An edge with no bends.
   */
  def this(start: VType, stop: VType, startPos: (Int, Int), stopPos: (Int, Int), originalEdge: EType[VType]) = this(start, stop, startPos, stopPos, Vector(), originalEdge)

  override def equals(other: Any) = other match {
    case d: DrawnEdge[VType, EType] => d.start == start && d.stop == stop && d.startPos == startPos && d.bends == bends && d.originalEdge == originalEdge
    case _ => false
  }

  override def hashCode: Int = start.## ^ stop.## ^ startPos.## ^ stopPos.## ^ bends.## ^ originalEdge.##
}
