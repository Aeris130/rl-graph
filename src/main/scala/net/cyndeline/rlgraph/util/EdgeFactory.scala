package net.cyndeline.rlgraph.util

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.OuterEdge

/**
 * Produces default edges between two vertices.
 */
trait EdgeFactory[VType, EType[X] <: UnDiEdge[X]] {
  def produce(from: VType, to: VType): EType[VType] with OuterEdge[VType, EType]
}
