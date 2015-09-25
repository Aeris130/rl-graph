package net.cyndeline.rlgraph.planarGraphDrawing.rectangular.dataStructure

/**
 * Represents vertices in a graph when computing its rectangular dual. Since doing so involves adding an additional
 * four vertices to the graph, this vertex can also be used to represent those.
 *
 * @param v The vertex in the original graph being represented, or None if this is of the additional vertices.
 * @param key A unique key used to distinguish one of the four additional vertices, or None if this object represents
 *            a regular vertex.
 * @param isOuter True if this is one of the four outer vertices, false if it is a split.
 */
class RVertex[VType] private (v: Option[VType], val key: Option[String], isOuter: Boolean) {

  def this(v: VType) = this(Some(v), None, false)

  /**
   * Constructs a dummy vertex used to represent the outer vertices.
   * @param key A unique key used to identify this vertex.
   * @param outer True if this is one of the four outer vertices N, S, W, E, otherwise false if it is a split.
   */
  private def this(key: String, outer: Boolean) = {
    this(None, Some(key), outer)
    require(!key.isEmpty, "Empty key specified.")
  }

  /** @return True if this object represents one of the four outer vertices or a split, otherwise false. */
  def isOuterVertex: Boolean = key.isDefined && isOuter

  /** @return True if this vertex resulted from splitting an edge to remove separating triangles, otherwise false. */
  def isSplit: Boolean = key.isDefined && !isOuter

  /** @return The vertex represented by this RVertex. Should only be called on RVertices that aren't outer or splits. */
  def vertex: VType = v.getOrElse(throw new Error("Cannot retrieve value from the outer vertex " + key.get))

  override def toString: String = if (isOuterVertex || isSplit) key.get.toString else v.get.toString

  override val hashCode: Int = if (key.isDefined) key.get.## else v.get.##

  override def equals(other: Any) = other match {
    case r: RVertex[VType] => if ((isOuterVertex && r.isOuterVertex) || (isSplit && r.isSplit)) key == r.key
      else if (!isOuterVertex && !r.isOuterVertex && !isSplit && !r.isSplit) vertex == r.vertex
      else false
    case _ => false
  }
}

object RVertex {
  def apply[VType](v: VType) = new RVertex(v)
  def outer[VType](key: String) = new RVertex[VType](key, true)
  def split[VType](key: String) = new RVertex[VType](key, false)
}
