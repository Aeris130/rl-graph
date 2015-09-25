package net.cyndeline.rlgraph.regularEdgeLabeling.angularMap

/**
 * A black or white vertex in an angular map.
 *
 * @param v The vertex represented by this AV, if it is black. Otherwise None.
 * @param id An id unique to the angular map this vertex is present in. Only used to ensure deterministically
 *           constructed maps.
 */
class AngularVertex[V] private (v: Option[V], val id: Int) {

  def this(v: V, id: Int) = this(Some(v), id)
  def this(id: Int) = this(None, id)

  val isBlack = v.isDefined
  val isWhite = v.isEmpty

  def vertex = v.getOrElse(throw new Error("Cannot retrieve vertex value from a white angular vertex."))

  override def equals(other: Any): Boolean = other match {
    case av: AngularVertex[V] => {
      if (isBlack && av.isBlack)
        vertex == av.vertex
      else if (isWhite && av.isWhite)
        id == av.id
      else
        false
    }
    case _ => false
  }

  override def hashCode: Int = id.## ^ v.##

  override val toString: String = {
    val builder = new StringBuilder()
    if (isWhite) {
      builder ++= "W(" + id + ")"
    } else
      builder ++= "B(" + vertex + ")"

    builder.toString()
  }
}

object AngularVertex {
  def apply[V](v: V, id: Int) = new AngularVertex(v, id)
  def apply[V](id: Int) = new AngularVertex[V](id)
}
