package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help

/**
 * Assigns id's to vertex wrappers to ensure their uniqueness even when they don't have a value stored.
 */
class VertexWrapperFactory[VType] {
  private var nextId = -1

  def createVertexWrapper(value: VType): VertexWrapper[VType] = {
    nextId += 1
    new VertexWrapper(nextId, value)
  }
  def createEmptyVertexWrapper: VertexWrapper[VType] = {
    nextId += 1
    new VertexWrapper[VType](nextId)
  }
}
