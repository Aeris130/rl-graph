package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.improvementHeuristic

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.Segment
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.improvementHeuristic.CornerType.CornerType

import scala.util.hashing.MurmurHash3

/**
 * Represents the beginning or end of a segment that points left->right or down->up.
 *
 * @constructor Constructs a new segment corner.
 * @param segment The segment being represented. Each segment has two corners.
 * @param mainCoordinate The coordinate each vertex in the segment is mapped again. For horizontal segments, it's
 *                       the y-value.
 * @param secondaryCoordinate Top or bottom coordinate on the axis opposite to the main coordinate. For horizontal
 *                            segments it's the x value.
 * @param cornerType Top if the secondary coordinate is the highest coordinate on the secondary axis, otherwise Bottom.
 */
case class SegmentCorner[VType](segment: Segment[VType], mainCoordinate: Int, secondaryCoordinate: Int, cornerType: CornerType) {

  override def equals(other: Any): Boolean = other match {
    case sc: SegmentCorner[VType] => sc.segment == segment && sc.mainCoordinate == mainCoordinate && sc.cornerType == cornerType
    case _ => false
  }

  override val hashCode: Int = segment.## ^ mainCoordinate.## ^ secondaryCoordinate.## ^ MurmurHash3.stringHash(cornerType.toString)
}

object CornerType extends Enumeration {
  type CornerType = Value
  val Top = Value("Top")
  val Bottom = Value("Bottom")
}
