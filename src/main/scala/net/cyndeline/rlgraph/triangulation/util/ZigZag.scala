package net.cyndeline.rlgraph.triangulation.util

import net.cyndeline.rlgraph.face.Face

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
 * Assigns additional edges to a face in order to make it triangulated.
 *
 * This class only handles the assigning of edges, and does not guarantee that multi-edges are prevented or that
 * the minimum degree of the graph as a whole increases by a constant.
 *
 * Basic algorithm description: Given a face with vertices v1 to vp, add the edges (vp, v2), (v2, vp-1), (vp-1, v3),
 * (v3, vp-2) ... (v[floor(p/2), v[floor(p/2)] + 2)
 *
 * @constructor Constructs a new ziz-zag edge addition.
 * @param face The face to triangulate. Must be a cycle where no vertex appears more than once.
 * @param first The first vertex to start the triangulation from (v1 in the algorithm).
 * @param last The last vertex to end the triangulation at (vp in the algorithm).
 * @tparam VType Vertex type used in the face.
 */
class ZigZag[VType : ClassTag](face: Face[VType], first: VType, last: VType) extends ZigZagI[VType] {
  if(face.vertexSize < 4)
    throw new IllegalArgumentException("Cannot zig-zag faces with less than 4 vertices.")
  else if (face.vertices.toSet.size != face.vertices.size)
    throw new IllegalArgumentException("Cannot zig-zag faces that aren't cycles (" + face + ").")

  /** Every edge needed to zig-zag the face. Edges will always go from a vertex on the lower half of n
    * (where the face contains n vertices) to a vertex on the upper half. Example: Given a face with
    * 7 vertices, all edges would go from 2,3 to 5,6,7: (2,7), (2,6), (3,6), (3,5). The edge (6,3)
    * would not be present as 6 is in the upper half, and 3 in the lower.
    *
    * Edges will also always go from 1 towards v[floor(p/2)]. In the previous example, this means
    * that (2,x) always comes before (3,x), (3,x) before (4,x) etc. With vertices on the upper half
    * it's the opposite. (x,7) always comes before (x,6) etc.
    */
  val additionalEdges: Vector[(VType, VType)] = computeEdges

  private def computeEdges: Vector[(VType, VType)] = {
    val nodesInFace: Array[VType] = rearrangeVertices
    var start = 1 // Second element in the face
    var end = nodesInFace.size - 1 // Last element in the face
    var updateStart = false
    val result = new ArrayBuffer[(VType, VType)]()

    while (start < end - 1) { // Start must have at least one vertex between it and end, or they're neighbors
      result += ((nodesInFace(start), nodesInFace(end)))

      if (updateStart) {
        start += 1
        updateStart = false
      } else {
        end -= 1
        updateStart = true
      }
    }

    result.toVector
  }

  /* First = 0, last = 5 in 3, 4, 5, 0, 1, 2 => 0, 1, 2, 3, 4, 5
   *
   */
  private def rearrangeVertices: Array[VType] = {
    // Split when finding the first vertex that violates the predicate
    val span: (Vector[VType], Vector[VType]) = face.vertices.span(_ != first)
    val faceWithFirstVertexAsHead = (span._2 ++ span._1).toArray

    if (faceWithFirstVertexAsHead(0) != first || faceWithFirstVertexAsHead(face.vertexSize - 1) != last)
      throw new Error("The face " + face + " could not be spanned such that " + first + " is at the head of the list, and " + last + " is the last element.")

    faceWithFirstVertexAsHead
  }
}

/**
 * Used for injection.
 */
trait ZigZagI[VType] {
  def additionalEdges: Vector[(VType, VType)]
}

object ZigZag {
  def apply[VType : ClassTag](face: Face[VType], first: VType, last: VType) = new ZigZag(face, first, last)
}
