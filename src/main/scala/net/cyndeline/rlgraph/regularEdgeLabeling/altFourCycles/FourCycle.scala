package net.cyndeline.rlgraph.regularEdgeLabeling.altFourCycles

import net.cyndeline.rlgraph.regularEdgeLabeling.angularMap.AngularMap
import net.cyndeline.rlgraph.regularEdgeLabeling.{EdgeLabeling, LabelEdge}

/**
 * An alternating essential 4-cycle in a regular embedding.
 *
 * The cycle is considered left-alternating if the left cycle-edge of the vertex has the same REL set membership as
 * the inner edges connected to it (every inner edge connected to a vertex is guaranteed to have the same set
 * membership, and every vertex with inner edges are guaranteed to match each others set memberships).
 */
sealed trait FourCycle[V] {

  /** @return The edges of the cycle, in no particular order. Note that these edges cannot be traversed in a directed
    *         circuit, as the 4-cycle traverses some of them in their opposite direction. */
  def outerEdges: Vector[Int]

  /** @return The edges inside the cycle, in no particular order. */
  def innerEdges: Vector[Int]

  /** @return A cycle that's marked as alternating in the opposite direction of what it does now. */
  def flip: FourCycle[V]

  def isRightAlternating: Boolean
  def isLeftAlternating: Boolean

}

/**
 * An alternating 4-cycle in a regular edge labeling, enclosing one or more edges.
 *
 * @param outerEdges Indices of the four outer edges of the cycle in the REL they were constructed from.
 * @param innerEdges Indices of every inner edge enclosed by the cycle from both T-sets in the REL they were constructed
 *                   from.
 * @param index A unique index in the 4-cycle collection this cycle belongs to.
 * @param leftAlternating True if this cycle is left-alternating, false if it is right-alternating.
 * @tparam V Vertex type of the cycle.
 */
class EssentialFourCycle[V] private (val outerEdges: Vector[Int],
                                     val innerEdges: Vector[Int],
                                     val index: Int,
                                     leftAlternating: Boolean) extends FourCycle[V] {
  def isRightAlternating: Boolean = !leftAlternating
  def isLeftAlternating: Boolean = leftAlternating

  /** @return A cycle that's marked as alternating in the opposite direction of what it does now. */
  def flip: EssentialFourCycle[V] = new EssentialFourCycle(outerEdges, innerEdges, index, !leftAlternating)

  override def equals(other: Any): Boolean = other match {
    case efc: EssentialFourCycle[V] => index == efc.index && outerEdges == efc.outerEdges &&
      innerEdges == efc.innerEdges && isLeftAlternating == efc.isLeftAlternating
    case _ => false
  }

  override def hashCode: Int = outerEdges.## ^ innerEdges.## ^ index
}

/**
 * Creates essential 4-cycles after computing the alternating direction of them.
 */
object EssentialFourCycle {

  /**
   * @param outerEdges The four outer edges of the cycle.
   * @param innerEdges Every inner edge enclosed by the cycle from both T-sets.
   * @param index A unique index in the 4-cycle collection this cycle belongs to.
   * @param rel The regular edge labeling used to compute the cycle.
   * @return An alternating 4-cycle.
   */
  def apply[V](outerEdges: Vector[LabelEdge[V]],
               innerEdges: Vector[LabelEdge[V]],
               index: Int,
               rel: EdgeLabeling[V],
               map: AngularMap[V]): EssentialFourCycle[V] = {
    require(outerEdges.size == 4, "The outer edge set of a 4-cycle must have size 4 (currently " + outerEdges.mkString(", ") + ")")
    require(!innerEdges.isEmpty, "Al alternating 4-cycle must have inner edges.")
    require(index >= 0, "The index of a 4-cycle must be >= 0 (currently " + index + ")")
    val outerVertices = outerEdges.map(e => Vector(e.from, e.to)).flatten.toSet
    val innerWithOuter = innerEdges.find(e => outerVertices.contains(e.from) || outerVertices.contains(e.to)).get
    val outer = if (outerVertices.contains(innerWithOuter.from)) innerWithOuter.from else innerWithOuter.to
    val inner = if (outer == innerWithOuter.from) innerWithOuter.to else innerWithOuter.from

    /* Move CC from the inner neighbor until an outer edge is found. If it matches the T-set membership of the
     * neighbor, the cycle is left-alternating. Otherwise right.
     *
     * If the 4-cycle only contains a single edge between two outer vertices, the loop wouldn't run, so the adjacency
     * is moved one step manually before beginning, which brings it to an outer edge immediately if this is the case.
     */
    var current = rel.originalEmbedding.embeddingFor(outer).entryFor(inner).previous
    while (!outerVertices.contains(current.adjacentVertex)) {
      current = current.previous
    }

    val outerEdge = outerEdges.find(e => {
      val vSet = Set(e.from, e.to)
      vSet.contains(outer) && vSet.contains(current.adjacentVertex)
    }).get
    val leftLeaning = rel.isMemberOfT1(innerWithOuter) == rel.isMemberOfT1(outerEdge)

    new EssentialFourCycle(outerEdges.map(_.index), innerEdges.map(_.index), index, leftLeaning)
  }
}