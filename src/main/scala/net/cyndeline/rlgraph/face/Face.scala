package net.cyndeline.rlgraph.face

import net.cyndeline.rlgraph.cycles.Cycle

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
 * Represents a single face in a planar graph, with vertices in the same order they're traversed
 * in the face. Allows the user to add data fields using the decoration pattern rather than inheritance, by adding
 * custom container object.
 *
 * Constructing the faces takes O(n) time, where n is the amount of vertex appearances in it.
 *
 * @constructor Creates a new face.
 * @param inputVertices Vertices of the face in the order they're traversed. The last vertex is connected to the first.
 * @param decoration Allows algorithms to store data in the face using the decorator pattern rather than inheritance.
 *                   None if no decoration exists.
 */
class Face[VType : TypeTag : ClassTag] private (inputVertices: Vector[VType], val decoration: Option[AnyRef]) {
  if (inputVertices.size < 2) throw new IllegalArgumentException("Face " + inputVertices + " contains " + inputVertices.size + " vertices, must have 2 or more.")

  /** This face represented as a cycle. */
  val cycle = Cycle(inputVertices)

  val vertices: Vector[VType] = inputVertices

  /* Includes an edge between the first and last vertex in the vertex list. */
  val edges: Vector[(VType, VType)] = cycle.edges

  private val edgeSet = edges.toSet

  // List takes O(n) time to check size
  private val vSize = cycle.length
  private val eSize = cycle.edgeSize

  /**
   * Constructs a new face.
   * @param vertices Vertices of the face in the order they're traversed. The last vertex is connected to the first.
   * @return A new face with no decoration.
   */
  def this(vertices: Vector[VType]) = this(vertices, None)

  /**
   * Constructs a new face and adds a decoration to it.
   * @param vertices Vertices of the face in the order they're traversed. The last vertex is connected to the first.
   * @param decoration User specified object that can be used to store algorithm-specific data.
   * @return A new face with an added decoration object.
   */
  def this(vertices: Vector[VType], decoration: AnyRef) = this(vertices, Some(decoration))

  /**
   * Checks if this face contains a specified edge.
   * @param edge Edge to look for.
   * @return True if the edge is a part of this face, otherwise false.
   */
  def containsEdge(edge: (VType, VType)): Boolean = edgeSet.contains(edge)

  /**
   * Checks if this face contains at least one edge present in the other face. This is done
   * by checking the reverse of edges in this face, since every edge is traversed in opposite
   * directions when parsing them from a planar embedding.
   *
   * @param otherFace Face to check common edges for.
   * @return True if the other face shares at least one edge, otherwise false.
   */
  def isAdjacentTo(otherFace: Face[VType]): Boolean = {
    val thisEdges = edges.iterator
    while (thisEdges.hasNext) {
      val edge = thisEdges.next()
      if (otherFace.containsEdge((edge._2, edge._1))) return true
    }

    false
  }

  def vertexSize = vSize
  def edgeSize = eSize

  /**
   * Adds a decoration to a face.
   * @param d Decoration to add.
   * @return A copy of this face with the decoration added.
   */
  def decorate(d: AnyRef): Face[VType] = new Face(vertices, Some(d))

  /** Maps every vertex in the face to the vertex that proceeds it when traversing the edges. */
  lazy val vertexPriorTo: Map[VType, VType] = edges.map(e => e._2 -> e._1).toMap

  /** Maps every vertex in the face to the vertex that comes after it when traversing the edges. */
  lazy val vertexPresentAfter: Map[VType, VType] = edges.map(e => e._1 -> e._2).toMap

  override def toString: String = {
    val builder = new StringBuilder()
    builder ++= "[Face " + vertices.mkString(", ")

    if (decoration.isDefined)
      builder ++= " " + decoration.get.toString

    builder ++= "]"
    builder.toString()
  }

  override def equals(other: Any): Boolean = other match {
    case f: Face[VType] => f.cycle == cycle
    case _ => false
  }

  override def hashCode = cycle.##

}

/**
 * Helper factory when instantiating faces manually (no need for lists).
 */
object Face {
  def apply[VType : Manifest](vs: VType*) = new Face(vs.toVector)
}
