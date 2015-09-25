package net.cyndeline.rlgraph.util

/**
 * Responsible for producing vertices for a graph based on an input object.
 */
trait VertexFactory[VType, Input] {

  /**
   * Produces a vertex.
   * @param input Object to base vertex on.
   * @return The created vertex.
   */
  def produceVertex(input: Input): VType
}
