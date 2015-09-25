package net.cyndeline.rlgraph.util.graphConverters.jbpt.hashStructure

import org.jbpt.graph.{Edge, Graph}
import org.jbpt.hypergraph.abs.Vertex

/**
 * A jbpt edge whose hash value is determined using its vertices.
 */
class EdgeHash(g: Graph, v1: Vertex, v2: Vertex) extends Edge(g, v1, v2) {
  override def hashCode: Int = v1.## ^ v2.##
}
