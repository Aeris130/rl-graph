package net.cyndeline.rlgraph.face

/**
 * Maps edges of a set of faces to the face that appears on its left/right side.
 *
 * Given an edge that goes from vertex A to B in the face F, F lies on the left side of the edge.
 *
 * @param faces Every face whose edge membership should be managed by this object.
 */
class FaceMembershipManager[VType](val faces: Vector[Face[VType]]) {
  private val facesForEdge: Map[(VType, VType), Face[VType]] = faces.map(f => f.edges.map(e => e -> f)).flatten.toMap

  /**
   * Retrieves the inner face on the left side of an edge as it traverses the face.
   * @param edge Tuple containing the vertices of an edge as they are traversed in the face.
   * @return The face traversed by the edge.
   */
  def leftFace(edge: (VType, VType)): Face[VType] = {
    val reverseEdge = (edge._2, edge._1)
    facesForEdge.get(reverseEdge).getOrElse(throw new NoSuchElementException("The edge " + reverseEdge + " is not a member of any face: " + facesForEdge))
  }

  def leftFace(from: VType, to: VType): Face[VType] = leftFace((from, to))

  /**
   * Retrieves the outer face on the right side of an edge as it traverses the edge opposite to the face.
   * @param edge Tuple containing the vertices of an edge as they are traversed on the opposite side in the face.
   * @return The opposite face traversed by the edge.
   */
  def rightFace(edge: (VType, VType)): Face[VType] = {
    val reverseEdge = (edge._2, edge._1)
    facesForEdge.get(edge).getOrElse(throw new NoSuchElementException("The edge " + edge + " is not a member of any face: " + facesForEdge))
  }

  def rightFace(from: VType, to: VType): Face[VType] = rightFace((from, to))

}
