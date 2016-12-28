package net.cyndeline.rlgraph.triangulation.fourConnectivity

import net.cyndeline.rlcommon.util.DoubleLinkedList
import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.planar.PlanarFaceAugmentation
import net.cyndeline.rlgraph.triangulation.fourConnectivity.help.{EdgeComputation, FaceSplit}
import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
 * Triangulates a planar biconnected graph without separating triangles (a triangle in the graph that is not a face)
 * while ensuring that it becomes 4-connected, and without introducing any additional separating triangles.
 *
 * The algorithm used is the one given by T.Biedl, G.Kant and M.Kaufmann in the article "On Triangulating Planar Graphs
 * under the Four-Connectivity Constraint". It was later shown by M.Ancona, S.Drago and P.A.Mazzarello in
 * "OCoRD: Optimal Constructor of a Rectangular Dual Software Documentation" that the original algorithm contained
 * a case where separating triangles could be introduced, and a modification was proposed that has also been included
 * in this implementation.
 */
class FourConnectivityTriangulation[VType: TypeTag : ClassTag] {
  private val faceAug = new PlanarFaceAugmentation()
  private val faceComp = new FaceComputation[VType]()

  /**
   * Triangulates every face in a graph except the ones specified by the user.
   * @param embedding A biconnected graph with no separating triangles.
   * @param ignoreFaces Every face in the input embedding that shouldn't be triangulated.
   */
  def triangulateWithoutFaces(embedding: Embedding[VType], ignoreFaces: Set[Face[VType]]): Embedding[VType] = augment(embedding, ignoreFaces)

  /**
   * Augments a graph embedding by adding edges until it becomes triangular and four-connected.
   * @param embedding A biconnected graph with no separating triangles.
   */
  def triangulate(embedding: Embedding[VType]): Embedding[VType] = augment(embedding, Set())

  private def augment(embedding: Embedding[VType], ignoreFaces: Set[Face[VType]]): Embedding[VType] = {
    var currentEmbedding = embedding
    val allFaces = new FaceComputation[VType]().computeFaces(currentEmbedding).filter(_.vertexSize > 3)
    val allNonTriangularFaces = allFaces diff ignoreFaces.toVector

    // Maps every vertex that is a member of a non-triangular face to the set of face it's a member of.
    val facesConnectedToVertex = vertexToFaceMap(allNonTriangularFaces)

    while(facesConnectedToVertex.nonEmpty) {
      val v = facesConnectedToVertex.keys.maxBy(degree(_, currentEmbedding))
      val face = facesConnectedToVertex(v).head // Arbitrary which face is selected
      val vertexList = new DoubleLinkedList(face.vertices:_*)
      vertexList.moveTo(v)

      /* Make sure that the degree of u1 >= u(p), regardless of which direction the face needs to be traversed in. */
      var reversed = false
      if (degree(vertexList.getPrevious, currentEmbedding) > degree(vertexList.getNext, currentEmbedding)) {
        vertexList.reverse()
        reversed = true
      }

      val uP = vertexList.getPrevious
      val u1 = vertexList.getNext

      val common = commonNeighbors(u1, uP, currentEmbedding)

      /* If v is the only neighbor of u1 and u(p), or the face only has 4 vertices and the
       * neighbors are the other two */
      if (common == Vector(v) || (face.vertexSize == 4 && common.size == 2 && common.toSet == (face.vertices diff Vector(uP, u1)).toSet)) {

        /* Add the single edge u1 ~ uP, then split the face and re-register any splits with size > 3. */
        currentEmbedding = GraphCommons.embedEdgeInFace(currentEmbedding, face, u1, uP)
        val faceSplits = new FaceSplit(face, u1, uP)

        deRegisterFace(face, facesConnectedToVertex)

        // Only one of the splits may (or may not) have size > 3 now, as [u1, uP, v] is the other split
        val otherFace = faceSplits.faces.find(_.vertexSize > 3)
        if (otherFace.isDefined) {
          registerFace(otherFace.get, facesConnectedToVertex)
        }

      } else { // Some common neighbor w discovered that needs to be taken into consideration when triangulating

        val w = common.find(_ != v).get // Some common neighbor of u1, uP

        // This is the part where OCoRD's suggested changes begins
        vertexList.moveTo(u1)

        /* Find the vertex i(j) j > 1, closest to v that doesn't have w as a neighbor,
         * when traversing from u2 -> u(p) -> u(p+1) == v. No such j means the graph had separating triangles after all.
         *
         * j must be different from w if w is on the face, otherwise the case j == v could never happen.
         */
        vertexList.next // u2, different from uP since the face has size > 3.
        val uJ = vertexList.find(uv => uv != w && !neighbors(uv, currentEmbedding).contains(w)).getOrElse {
          throw new IllegalArgumentException("Input graph contains a star with central vertex in " + w + ", cannot be triangulated while adhering to the planar 4-connectivity constraint.")
        }

        if (uJ == v) {

          /* If F contains w, and w and v has no common adjacencies (other than the neighbors of w on F if f has size 4),
           * an edge should be added from v to w when triangulating the face. Otherwise v, w and w's two neighbors make
           * up the remaining face after the triangulation: [v, n1, w, n2].
           */
          val f = if (!reversed) face else new Face(face.vertices.reverse)
          val addEdgeFromVtoW: Boolean = {
            if (face.vertices.contains(w)) {
              (commonNeighbors(uJ, w, currentEmbedding) diff Vector(f.vertexPriorTo(w), f.vertexPresentAfter(w))).isEmpty

            } else {
              false
            }
          }

          // add edges
          val edgesToAdd = EdgeComputation.ujEqualsV(v, u1, uP, w, face, addEdgeFromVtoW)
          currentEmbedding = augmentEdges(edgesToAdd.edges, face, currentEmbedding, facesConnectedToVertex)

        } else {

          val edgesToAdd = EdgeComputation.maximalK(v, u1, uP, uJ, w, face, currentEmbedding)
          currentEmbedding = augmentEdges(edgesToAdd.edges, face, currentEmbedding, facesConnectedToVertex)
        }
      }
    }

    currentEmbedding
  }

  private def augmentEdges(edges: Vector[(VType, VType)],
                           face: Face[VType],
                           embedding: Embedding[VType],
                           faceMap: mutable.HashMap[VType, mutable.HashSet[Face[VType]]]): Embedding[VType] = {
    var currentEmbedding = embedding
    currentEmbedding = faceAug.embedEdges(edges, face, currentEmbedding)
    val resultingFaceGaps = facesOfEdges(edges, currentEmbedding).filter(_.vertexSize > 3)
    deRegisterFace(face, faceMap)

    for (newFace <- resultingFaceGaps) {
      registerFace(newFace, faceMap)
    }

    currentEmbedding
  }

  private def vertexToFaceMap(faces: Vector[Face[VType]]): mutable.HashMap[VType, mutable.HashSet[Face[VType]]] = {
    val hm = new mutable.HashMap[VType, mutable.HashSet[Face[VType]]]()

    for (f <- faces; v <- f.vertices) {
      if (hm.contains(v)) {
        hm += v -> (hm(v) + f)
      } else {
        val set = new mutable.HashSet[Face[VType]]()
        set.add(f)
        hm += v -> set
      }
    }

    hm
  }

  private def deRegisterFace(face: Face[VType], faceMap: mutable.HashMap[VType, mutable.HashSet[Face[VType]]]) {
    for (vertexInFace <- face.vertices) {
      val currentFaces = faceMap(vertexInFace)
      currentFaces.remove(face)

      if (currentFaces.isEmpty)
        faceMap.remove(vertexInFace)
    }
  }

  private def registerFace(face: Face[VType], faceMap: mutable.HashMap[VType, mutable.HashSet[Face[VType]]]) {
    for (vertexInFace <- face.vertices) {
      val currentFaces = faceMap.getOrElse(vertexInFace, {
        val newSet = new mutable.HashSet[Face[VType]]()
        faceMap += vertexInFace -> newSet
        newSet
      })
      currentFaces.add(face)
    }
  }

  private def commonNeighbors(a: VType, b: VType, embedding: Embedding[VType]): Vector[VType] = {
    require(a != b)
    val an = neighbors(a, embedding)
    val bn = neighbors(b, embedding)
    an intersect bn
  }

  private def facesOfEdges(edges: Vector[(VType, VType)], emb: Embedding[VType]): Vector[Face[VType]] = {
    val allFaces = for (e <- edges) yield Vector(faceComp.computeSingleFace(e._1, e._2, emb), faceComp.computeSingleFace(e._2, e._1, emb))
    allFaces.flatten.distinct
  }

  private def neighbors(v: VType, embedding: Embedding[VType]): Vector[VType] = embedding.embeddingFor(v).toVector.map(_.adjacentVertex)

  private def degree(v: VType, embedding: Embedding[VType]): Int = embedding.embeddingFor(v).size
}
