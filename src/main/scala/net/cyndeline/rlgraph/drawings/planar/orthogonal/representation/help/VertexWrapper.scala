package net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help

/**
 * Wraps a vertex from a graph. Used as vertices for Darts when computing orthogonal representations, since
 * removing edge bends and adding dummy vertices during rectangularization sometimes requires extra vertices
 * that doesn't have an equivalent in the original graph.
 *
 * @constructor Construct a new wrapper around a vertex.
 * @param value Vertex to wrap.
 */
class VertexWrapper[VType](val id: Int, val value: Option[VType]) {
  def this(id: Int) = this(id, None)
  def this(id: Int, v: VType) = this(id, Option(v))

  def isDefined: Boolean = value.isDefined

  override def equals(other: Any): Boolean = other match {
    case v: VertexWrapper[VType] => v.## == this.## && v.value == value && v.id == id
    case _ => false
  }

  override def hashCode: Int = id.## ^ value.##

  override val toString: String = {
    val builder = new StringBuilder()
    if (value.isDefined)
      builder ++= value.get.toString
    else {
      val hStr = this.hashCode.toString
      builder ++= "{dummy_wrapper "
      builder ++= hStr
      builder ++= "}"
    }

    builder.toString()
  }
}
