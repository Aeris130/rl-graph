package net.cyndeline.rlgraph.util.graphConverters.jgraphed

import graphStructure.EdgeExtender

/**
 * Extends the JGraphEd edge and adds the ability to store the user-supplied edge inside JGraphEd.
 */
class EdgeWrapper[EType] extends EdgeExtender {
  private var originalEdge: Option[EType] = None

  def setEdge(edge: EType) {
    originalEdge = Some(edge)
  }

  def edge: EType = originalEdge.getOrElse(throw new Error("Original edge not set in JGraphEd edge extender."))

}
