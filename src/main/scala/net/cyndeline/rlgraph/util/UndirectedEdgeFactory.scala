package net.cyndeline.rlgraph.util

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

/**
 * Copies undirected edges.
 *
 * @constructor Creates a new undirected edge copier.
 */
class UndirectedEdgeFactory[VType] extends EdgeCopyFactory[VType, UnDiEdge] {

  /**
   * Copies an edge and assigns the copy new vertices.
   *
   * @param edge Old edge to copy.
   * @param a A vertex to assign the new edge. If the edge is directed, this is the .from value.
   * @param b A vertex to assign the new edge. If the edge is directed, this is the .to value.
   * @return a copy of the edge with new vertices assigned. If the same copying is performed multiple times,
   *         an equal copy is returned every time.
   */
  def copyEdge(edge: UnDiEdge[VType], a: VType, b: VType): UnDiEdge[VType] with OuterEdge[VType, UnDiEdge] = a~b

}
