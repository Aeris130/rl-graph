package net.cyndeline.rlgraph.util.graphConverters.jbpt.hashStructure

import java.util

import org.jbpt.graph.{Edge, Graph}
import org.jbpt.hypergraph.abs.Vertex

/**
 * Overrides the edge method in jbpt's undirected graph class with one that creates edges with a fixed hash value.
 * This allows jbpt to produce the same SPQR tree over multiple calls with the same graph, as the order of vertices
 * and edges otherwise depends on random hashes assigned to the default vertex and edge classes used by jbpt.
 */
class JBPTHashGraph extends Graph {
  override def addEdge(v1: Vertex, v2: Vertex): Edge = {

    /* All code in this method (save for the EdgeHash instantiation) is just a port from the java code in the
     * original Graph class. Although this method only returns an edge, calling it will also cause the jbpt
     * graph to add the returned edge to itself.
     */

    val al = new util.ArrayList[Vertex]()
    al.add(v1)
    al.add(v2)
    val edges = this.getEdges(v1, v2)
    if (edges.size > 0) {
      val edgeIt = edges.iterator()
      while (edgeIt.hasNext) {
        val e = edgeIt.next()
        if (e.getVertices.size == 2)
          return null
      }
    }

    new EdgeHash(this, v1, v2)
  }
}
