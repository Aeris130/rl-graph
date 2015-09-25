package net.cyndeline.rlgraph.planarGraphDrawing.rectangular.help

import net.cyndeline.rlcommon.util.FrontInterval
import net.cyndeline.rlgraph.embedding.{Embedding, Vertex}
import net.cyndeline.rlgraph.face.Face

import scala.reflect.ClassTag

/**
 * Computes edge additions to the outer face of a PTP graph, fulfilling the requirement of the rectangular dual
 * algorithm that the outer face should have exactly four vertices, with every inner face being a triangle.
 *
 * The vertices on the outer face are grouped evenly between the four vertices (if only three vertices exists,
 * one of the four will connect to the same as two of the others). The vertices will also be embedded such that
 * the clockwise order of vertices around the northern vertex is west -> east -> inner vertices.
 *
 * @param outerFace The current outer face of a PTP graph to augment with 4 new outer vertices.
 * @param vS The new southern vertex on the outer face. If the input face only contains 3 vertices, this vertex will
 *           receive one edge less than the rest.
 * @param vN The new Northern vertex on the outer face.
 * @param vW The new western vertex on the outer face.
 * @param vE The new eastern vertex on the outer face.
 */
class OuterFaceAugmentation[V : ClassTag](outerFace: Face[V], vN: V, vE: V, vS: V, vW: V) {
  require(outerFace.vertexSize >= 3)

  /**
   * @param embedding An embedding containing the specified outer face.
   */
  def augmentEmbedding(embedding: Embedding[V]): Embedding[V] = {
    var currentEmbedding = embedding
    val outerEdges = Vector((vN, vE), (vE, vS), (vS, vW), (vW, vN))

    for (outer <- outerEdges)
      currentEmbedding = currentEmbedding.embedEdge(Vertex(outer._1) withDefaultPositionInVertex outer._2 withDefaultInsertPosition)

    if (outerFace.vertexSize < 4) {
      currentEmbedding = embed3Vertexface(currentEmbedding)
    } else {

      // Every vertex mapped to the vertex that comes before it in the face
      val nextVertex: Map[V, V] = (outerFace.vertices zip (outerFace.vertices.tail :+ outerFace.vertices.head)).toMap
      val vertexIntervals: Array[Array[V]] = FrontInterval(outerFace.vertices, 4)
        .map(_.toArray) // Speed up access to the first and last element in each interval
        .toArray
      val vertices = Array(vS, vE, vN, vW)

      /*
       * Due to the way grouping works, every outer vertex needs to connect to every vertex in its interval, and also
       * to the first vertex in the next interval, since each "corner" vertex needs to connect to two outer vertices.
       */
      for (i <- 0 to 3) {

        /* It's possible to use the next outer vertex as insert point for every edge by starting at the last
         * interval-vertex and inserting backwards towards the start.
         */
        val insertPosOnOuterFace = vertices(nextIndex(i))

        val currentInterval = vertexIntervals(i)
        val nextInterval = vertexIntervals(nextIndex(i))

        for (v <- currentInterval) {
          currentEmbedding = embedEdge(vertices(i), v, insertPosOnOuterFace, nextVertex(v), currentEmbedding)
        }

        /* A special case needs to be handled if this is the last edge for the last outer face. In this case, the
         * insert position for the inner vertex will no longer be the next vertex on the initial outer face, but
         * the north vertex.
         */
        val innerVertexInsertPos = if (i == 3) {
          vS
        } else {
          nextVertex(nextInterval(0))
        }

        currentEmbedding = embedEdge(vertices(i), nextInterval(0), insertPosOnOuterFace, innerVertexInsertPos, currentEmbedding)

      }
    }

    currentEmbedding
  }

  private def nextIndex(i: Int) = if (i == 3) 0 else i + 1

  /**
   * @param embedding An embedding that has the new outer vertices already embedded and connected.
   */
  private def embed3Vertexface(embedding: Embedding[V]): Embedding[V] = {
    val v1 = outerFace.vertices(0)
    val v2 = outerFace.vertices(1)
    val v3 = outerFace.vertices(2)

    embedding.embedEdge(Vertex(vS) withInsertPosition v2 inVertex v1 withInsertPosition vW)
      .embedEdge(Vertex(vS) withInsertPosition v3 inVertex v2 withInsertPosition v1)
      .embedEdge(Vertex(vE) withInsertPosition v3 inVertex v2 withInsertPosition vS)
      .embedEdge(Vertex(vE) withInsertPosition v1 inVertex v3 withInsertPosition v2)
      .embedEdge(Vertex(vN) withInsertPosition v1 inVertex v3 withInsertPosition vE)
      .embedEdge(Vertex(vW) withInsertPosition v1 inVertex v3 withInsertPosition vN)
      .embedEdge(Vertex(vW) withInsertPosition vS inVertex v1 withInsertPosition v3)
  }

  private def embedEdge(outer: V, inner: V, nextOuter: V, previousInner: V, embedding: Embedding[V]): Embedding[V] = {
    val ccOfNextOuter = embedding.embeddingFor(outer).entryFor(nextOuter).previous.adjacentVertex
    embedding.embedEdge(Vertex(outer) withInsertPosition previousInner inVertex inner withInsertPosition ccOfNextOuter)
  }
}
