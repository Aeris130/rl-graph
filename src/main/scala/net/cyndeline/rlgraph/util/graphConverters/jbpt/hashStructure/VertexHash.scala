package net.cyndeline.rlgraph.util.graphConverters.jbpt.hashStructure

import org.jbpt.hypergraph.abs.Vertex

/**
 * Specifies a hash value for jbpt vertices.
 */
class VertexHash(val id: Int) extends Vertex(id.toString) {

  override def toString: String = "JBPTVertex[" + id + "]"
  override def equals(other: Any): Boolean = other match {
    case v: VertexHash => v.id == id
    case _ => false
  }
  override def hashCode: Int = id
}
