package net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.help

import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.VertexWrapper

import scala.util.hashing.MurmurHash3

/**
 * A list of maximally connected vertices in a directed orthogonal representation.
 *
 * @constructor Creates a new segment.
 * @param vertices A list of connected vertices in the segment, in the order they're connected.
 * @param orientation Which direction the segment lies in (horizontal or vertical).
 * @param coordinate The coordinate of this segment on the axis opposite of the one it lies on. Example: A horizontal
 *                   segment has its y-coodinate here.
 */
class Segment[VType](val vertices: Vector[VertexWrapper[VType]], val orientation: SegmentOrientation, val coordinate: Int = 0) {
  private val set = vertices.toSet
  private val hash = MurmurHash3.stringHash(vertices.toString + orientation.toString)

  /**
   * Creates a segment with default coordinate 0.
 *
   * @param orientation Which direction the segment lies in (horizontal or vertical).
   * @param vertices A list of connected vertices in the segment, in the order they're connected.
   * @return A segment with the specified vertices and orientation, as well as the coordinate 0.
   */
  def this(orientation: SegmentOrientation, vertices: VertexWrapper[VType]*) = this(vertices.toVector, orientation)

  /**
   * Checks if a vertex is contained in the segment.
 *
   * @param vertex Vertex to look for.
   * @return True if the vertex is present in the segment, otherwise false.
   */
  def contains(vertex: VertexWrapper[VType]) = set.contains(vertex)

  /**
   * Assigns a new coordinate to a copy of this segment.
 *
   * @param c Coordinate to assign.
   * @return a copy of this segment with the specified coordinate.
   */
  def newCoordinate(c: Int): Segment[VType] = new Segment[VType](vertices, orientation, c)

  override def toString: String = "Seg(" + orientation + ", " + coordinate + "):" + vertices

  override def equals(other: Any): Boolean = other match {
    case s: Segment[VType] => s.vertices == vertices && s.orientation == orientation // Don't compare coordinate
    case _ => false
  }

  override def hashCode: Int = hash

}
