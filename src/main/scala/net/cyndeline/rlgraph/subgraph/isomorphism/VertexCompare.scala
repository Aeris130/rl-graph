package net.cyndeline.rlgraph.subgraph.isomorphism

/**
 * Compares vertices in a pattern against vertices in the graph being matched against the pattern.
 * @tparam V1 Vertex type in the graph to match the pattern against.
 * @tparam V2 Vertex type of the pattern graph.
 */
trait VertexCompare[V1, V2] {

  /**
   * Compares a vertex from a graph to a vertex in the pattern that is being matched against the graph.
   * @param v1 Vertex in the graph that is matched against a pattern.
   * @param v2 Vertex in the pattern.
   * @return True if v1 should be considered equal to v2, otherwise false.
   */
  def compareNode(v1: V1, v2: V2): Boolean

}
