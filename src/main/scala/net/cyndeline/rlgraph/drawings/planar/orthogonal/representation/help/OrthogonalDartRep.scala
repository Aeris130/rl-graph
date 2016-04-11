package net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help


/**
 * Stores a pre-drawing representation of an orthogonal graph as a set of faces made up of darts.
 *
 * @constructor Creates a new orthogonal dart representation.
 * @param faces All dart faces of the graph.
 * @param externalFace The face that has been assigned to be the external face in the drawing.
 * @param simplification A map of every edge-pair of vertices in their wrappers to the darts representing that edge.
 */
case class OrthogonalDartRep[VType]
  (faces: Set[OFace[VType]],
   externalFace: OFace[VType],
   simplification: Map[(VertexWrapper[VType], VertexWrapper[VType]), Vector[Dart[VType]]]) {
  if (!faces.contains(externalFace))
    throw new IllegalArgumentException("The set of orthogonal faces " + faces + " did not contain the external face " + externalFace)

  /**
   * Creates a new representation without dart representations.
   * @param faces All dart faces of the graph.
   * @param externalFace The face that has been assigned to be the external face in the drawing.
   * @return A new orthogonal dart representation with simplifications.
   */
  def this(faces: Set[OFace[VType]], externalFace: OFace[VType]) = this(faces, externalFace, Map())

  /**
   * Retrieves a dart from any of the faces of this representation.
   *
   * @param from Start wrapper.
   * @param to Stop wrapper.
   * @return A dart containing the start/stop vertices if one exists, otherwise None.
   */
  def getDart(from: VertexWrapper[VType], to: VertexWrapper[VType]): Option[Dart[VType]] = {
    for (face <- faces) {
      val d = face.getDart(from, to)
      if (d.isDefined) return d
    }

    None
  }

  /**
   * Returns all darts in the representation.
   * @return all darts in the representation.
   */
  def allDarts: Vector[Dart[VType]] = (for {
    face <- faces.toVector
  } yield face.darts).flatten

}
