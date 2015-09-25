package rlgraph.help

import net.cyndeline.rlgraph.util.EdgeFactory

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

/**
 * Produces undirected edges.
 */
class UndirectedEdgeFactory[VType] extends EdgeFactory[VType, UnDiEdge] {
  def produce(from: VType, to: VType): UnDiEdge[VType] with OuterEdge[VType, UnDiEdge] = from~to
}
