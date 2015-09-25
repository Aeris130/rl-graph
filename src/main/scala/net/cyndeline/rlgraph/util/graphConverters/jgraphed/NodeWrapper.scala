package net.cyndeline.rlgraph.util.graphConverters.jgraphed

import graphStructure.NodeExtender

/**
 * Wraps a user-supplied vertex inside JGraphEd.
 */
class NodeWrapper[VType] extends NodeExtender {
  private var originalVertex: Option[VType] = None

  def setVertex(vertex: VType) {
    originalVertex = Some(vertex)
  }

  def vertex: VType = originalVertex.getOrElse(throw new Error("Original vertex not set in JGraphEd node extender."))

}
