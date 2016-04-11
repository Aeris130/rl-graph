package net.cyndeline.rlgraph.drawings.planar.orthogonal.representation

import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.{OrthogonalDartRep, _}

import scalax.collection.GraphEdge.UnDiEdge

/**
 * Computes a simple orthogonal representation of a face (no edge bends).
 *
 * @constructor Creates a new orthogonal face simplifier object.
 */
class OrthogonalFaceSimplifier[VType, EType[X] <: UnDiEdge[X]](vertexWrapperFactory: VertexWrapperFactory[VType]) {
  private val dartSimplifier = new DartSimplifier[VType](vertexWrapperFactory)

  /**
   * Simplifies every face in an orthogonal representation.
   *
   * @param entireRepresentation Orthogonal representation to simplify.
   * @return a new orthogonal representation with every face simplified.
   */
  def simplifyFaces(entireRepresentation: OrthogonalDartRep[VType]): OrthogonalDartRep[VType] = {
    val faces = entireRepresentation.faces.iterator
    var simplifications = Map[(VertexWrapper[VType], VertexWrapper[VType]), Vector[Dart[VType]]]()
    var currentFaces = entireRepresentation.faces
    var externalFace = entireRepresentation.externalFace

    while (faces.hasNext) {
      val face = faces.next()
      val latestRepresentation = new OrthogonalDartRep[VType](currentFaces, externalFace, simplifications)
      val simpleFaceData: (OFace[VType], Map[(VertexWrapper[VType], VertexWrapper[VType]), Vector[Dart[VType]]])
        = simplifyFace(face, latestRepresentation)

      val simpleFace = simpleFaceData._1
      val simplifiedDarts = simpleFaceData._2

      if (face == externalFace)
        externalFace = simpleFace

      currentFaces -= face
      currentFaces += simpleFace
      simplifications = simplifications ++ simplifiedDarts
    }

    new OrthogonalDartRep[VType](currentFaces, externalFace, simplifications)
  }

  /**
   * Simplifies every dart in an orthogonal face by replacing all its bends (if any) with
   * multiple darts having the edge-bend degrees as vertex-bends between each other instead.
   *
   * @param face The face to simplify.
   * @param entireRepresentation The entire orthogonal representation that the face belongs to. The reason
   *                             is that a dart with bends on the inside of its face cannot be used alone to find
   *                             all the bends on the face that the dat represents. The opposite dart is needed as well.
   * @return a new orthogonal face where any edge-bends have been replaced with darts, using empty
   *         wrappers to represent their to/from vertices. Every dart that ended up being simplified is also
   *         mapped against the new dummy-vertices used in the new darts, in the order they were used (simplifying
   *         the opposite dart must be done by using those same vertices, but in the opposite direction).
   */
  def simplifyFace(face: OFace[VType], entireRepresentation: OrthogonalDartRep[VType]): (OFace[VType], Map[(VertexWrapper[VType], VertexWrapper[VType]), Vector[Dart[VType]]]) = {
    val result = simplify(face.darts.toList, entireRepresentation, entireRepresentation.simplification.map(e => e._1 -> e._2.toList), Nil)
    (new OFace(result._1.toVector), result._2.map(entry => entry._1 -> entry._2.toVector))
  }

  /**
   * Simplifies a face, one dart at the time. The dart (if no simplication was possible) or its simplified list
   * is added to the final result.
   *
   * @param face Darts remaining in the face to simplify.
   * @param orthogonalRepresentation Representation containing every dart in the graph.
   * @param simplifiedDarts Maps every original dart pair to the list of darts it was simplified into.
   * @param finalFace The face of darts after some of them potentially have been decomposed into sub-darts.
   * @return The new face as a list of darts, as well as a map over all darts that were simplified.
   */
  //TODO rewrite using vectors
  private def simplify(face: List[Dart[VType]],
                                orthogonalRepresentation: OrthogonalDartRep[VType],
                                simplifiedDarts: Map[(VertexWrapper[VType], VertexWrapper[VType]), List[Dart[VType]]],
                                finalFace: List[Dart[VType]]
                               ): (List[Dart[VType]], Map[(VertexWrapper[VType], VertexWrapper[VType]), List[Dart[VType]]]) = face match {
    case (head: Dart[VType]) :: tail => {
      val simplification: Option[List[Dart[VType]]] = simplifiedDarts.get(head.to, head.from)

      /**
       * Checking to see if the simplification is present comes first, since not finding it guarantees that the edge
       * is unsimplified even if it occurs later on in this face.
       */
      val simpleDartList: List[Dart[VType]] =
        if (simplification.isDefined) {
          dartSimplifier.simplifySimpleDart(head, simplification.get.toVector).toList

        } else {
          val oppositeDart = orthogonalRepresentation.getDart(head.to, head.from)
          if (!oppositeDart.isDefined)
            throw new IllegalArgumentException("No opposite dart for " + head + "found in " + orthogonalRepresentation.allDarts + ", despite no simplification mapped.")

          dartSimplifier.simplifyDart(head, orthogonalRepresentation.getDart(head.to, head.from).get).toList
        }

      if (simpleDartList.isEmpty)
        simplify(tail, orthogonalRepresentation, simplifiedDarts, finalFace :+ head)
      else
        simplify(tail, orthogonalRepresentation, simplifiedDarts + ((head.from, head.to) -> simpleDartList), finalFace ::: simpleDartList)
    }
    case Nil => (finalFace, simplifiedDarts)
  }

}
