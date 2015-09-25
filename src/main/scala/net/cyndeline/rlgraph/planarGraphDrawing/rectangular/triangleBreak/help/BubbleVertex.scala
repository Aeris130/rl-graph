package net.cyndeline.rlgraph.planarGraphDrawing.rectangular.triangleBreak.help

import net.cyndeline.rlcommon.util.UnorderedPair

/**
 * Immutable edge representation that doesn't depend on vertex order in the edge.
 *
 * @param e The original user data to construct the vertex from.
 */
class BubbleVertex[V] private (e: (V, V), bubbleAmount: Int) {
  require(e._1 != e._2, "Cannot create bubble vertices from self loop: " + e._1 + " to " + e._2 + ".")
  private val edge = UnorderedPair(e._1, e._2)
  private var vWeight: Int = 0

  /** One of the vertices in the edge represented by this bubble-vertex. */
  val _1: V = edge._1

  /** The vertex opposite of _1 in this bubble-vertex. */
  val _2: V = edge._2

  /**
   * @param w Weight to add to this vertex.
   * @return A copy of the vertex with the weight added.
   */
  def addWeight(w: Int): Unit = {
    vWeight += w
  }

  /**
   * @param w Weight to remove from this vertex.
   * @return A copy of the vertex with the weight removed.
   */
  def removeWeight(w: Int): Unit = {
    require(weight > 0, "Cannot reduce a bubble vertex to negative weight.")
    vWeight -= w
  }

  /** @return How many bubbles this vertex currently is a member of. */
  def weight: Int = vWeight

  override def equals(other: Any): Boolean = other match {
    case bv: BubbleVertex[V] => this._1 == bv._1 && this._2 == bv._2
    case _ => false
  }

  override val hashCode: Int = (_1, _2).##

  override val toString: String = "BubbleVertex(" + _1 + " - " + _2 + ")"
}

/**
 * Used to create new bubbles.
 */
object BubbleVertex {

  /**
   * Creates a bubble identical to every other bubble with the same vertices (regardless of order).
   * @param a A vertex in a graph edge.
   * @param b The opposite vertex in the edge. Cannot be the same as 'a.
   * @tparam V Vertex type.
   * @return A new bubble representing the vertices, having weight 0.
   */
  def apply[V](a: V, b: V): BubbleVertex[V] = new BubbleVertex((a, b), 0)
}
