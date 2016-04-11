package net.cyndeline.rlgraph.drawings.planar.orthogonal.representation

import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.darts.DartDirection._
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.{Dart, OFace, OrthogonalDartRep, VertexWrapper}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Computes directions for darts based on their vertex degrees, assuming that they traverse a face clockwise,
 * with the inside of the face to the right.
 *
 * @constructor Creates a new direction pointer.
 */
class DirectionPointer {

  /**
   * Computes directions for darts of a single face.
   *
   * @param face Face whose darts should be assigned directions.
   * @param start Since directions are arbitrary, this dart will be assumed to point in a direction specified by
   *              the user.
   * @param direction Direction that the specified dart should point in.
   * @return a new face with copies of the darts, this time assigned a direction.
   */
  def computeDirection[VType](face: OFace[VType], start: Dart[VType], direction: DartDirection): OFace[VType] = {
    var currentDirection = direction
    val darts = face.iteratorFrom(start)
    val newFace = new ArrayBuffer[Dart[VType]]()

    while (!darts.isEmpty) {
      val dart = darts.next()
      newFace += dart.newDirection(currentDirection)

      currentDirection = currentDirection.turn(dart.nextDegree)
    }

    new OFace(newFace.toVector)
  }

  /**
   * Computes directions for a set of faces, with opposite pointing darts receiving opposite directions.
   *
   * @param representation All faces to compute directions for. Every dart must have an opposite equivalent in some
   *                       face belonging to this set.
   * @param start Since directions are arbitrary, this dart will be assumed to point in a direction specified by
   *              the user.
   * @param startDirection Direction that the specified dart should point in.
   * @return a new set of faces with directions assigned.
   */
  def computeDirection[VType](representation: OrthogonalDartRep[VType], start: Dart[VType], startDirection: DartDirection): OrthogonalDartRep[VType] = {
    var remainingFaces = representation.faces
    val newFaces = new mutable.HashSet[OFace[VType]]()
    var externalFace = representation.externalFace

    /* If this collection contains a vertex-pair for a dart, the dart with the pair occurring in that order has
     * been directed, but not the opposite dart.
     */
    var mappedDirections = Map[(VertexWrapper[VType], VertexWrapper[VType]), DartDirection]()
    var direction = startDirection

    while (!remainingFaces.isEmpty) {
      var startDart: Dart[VType] = null

      var face = if (mappedDirections.isEmpty) {
        startDart = start
        findFaceWithDartOppositeTo(start.to, start.from, remainingFaces) // Might as well re-use this method to find the start face
      } else {
        val directedPair: ((VertexWrapper[VType], VertexWrapper[VType]), DartDirection) = mappedDirections.head // Arbitrary directed dart
        val dartVertices = directedPair._1
        direction = directedPair._2.opposite
        val f = findFaceWithDartOppositeTo(dartVertices._1, dartVertices._2, remainingFaces)
        startDart = f.getDart(dartVertices._2, dartVertices._1).get

        f
      }

      remainingFaces -= face
      val directedFace = computeDirection[VType](face, startDart, direction)
      newFaces += directedFace

      if (face == representation.externalFace)
        externalFace = directedFace

      /* Store the direction assigned temporarily so that each opposite dart can find and reverse it. */
      for (dart <- directedFace.darts) {
        val vertexPair = (dart.to, dart.from) // Opposite of the current dart
        if (mappedDirections.contains(vertexPair)) {
          mappedDirections -= vertexPair
        } else {
          mappedDirections += ((dart.from, dart.to) -> dart.direction.get)
        }
      }
    }

    if (!mappedDirections.isEmpty)
      throw new Error("The darts opposite to " + mappedDirections + " has not been processed.")

    new OrthogonalDartRep[VType](newFaces.toSet, externalFace)
  }

  private def findFaceWithDartOppositeTo[VType](a: VertexWrapper[VType], b: VertexWrapper[VType], faces: Set[OFace[VType]]): OFace[VType] = {
    for (face <- faces)
      if (face.contains(b, a))
        return face

    throw new Error("Face set " + faces + " did not contain a dart opposite to " + (a, b))
  }

}
