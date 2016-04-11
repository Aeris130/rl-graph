package net.cyndeline.rlgraph.drawings.planar.rectangular.embedding.help

import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation}

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
 * Merges biconnected components on the outer face.
 * @param selectOuterFace Function that selects a face to be used as outer face, or None if no matching face exists.
 * @tparam VType Vertex type in the embedding.
 */
class OuterEmbeddingMerger[VType : TypeTag : ClassTag](selectOuterFace: Vector[Face[VType]] => Option[Face[VType]]) {
  private val faceComputation = new FaceComputation[VType]()

  /**
   * @param embeddings A list of planar embeddings. Every embedding must have at least one face matching the
   *                   supplied outer-face function, or merging on the outer face will not be possible.
   * @param cutpointsToEmbedding Vertices where two or more embeddings intersect on their outer faces, mapped to
   *                             every intersecting embedding.
   * @param embeddingToCutpoints Embeddings mapped to every cutpoint that appear on its outer face.
   * @return An embedding where every adjacency in the initial list is present, and the unbounded face traverses
   *         every vertex in the lists unbounded faces.
   */
  def mergeEmbeddings(embeddings: Vector[Embedding[VType]],
                      cutpointsToEmbedding: Map[VType, Set[Embedding[VType]]],
                      embeddingToCutpoints: Map[Embedding[VType], Set[VType]]): Embedding[VType] = {
    if (embeddings.length < 2)
      return embeddings.head

    val processedCutVertices = new mutable.HashSet[VType]()
    val processedEmbeddings = new mutable.HashSet[Embedding[VType]]()

    /* Outer faces for each individual component. The outer face is used to find the incoming and outgoing edges
     * of a cutpoint on the face when traversing it in a clockwise direction. When embedding another component
     * to the cutpoint, it should be placed after the incoming edge (and before the outgoing) to ensure that the
     * component is embedded on the outside, and not the inside of the component.
     */
    val faces = (for (e <- embeddings) yield e -> findOuterFace(e)).toMap

    /* For each cutpoint on a face, the vertex that lies before it on the outer face must be found. The adjacency
     * entry for that vertex will serve as the insertion point for other components.
     *
     * Embedding -> Map: Cut point -> prior vertex
     */
    val verticesPriorToCutpoints: Map[Embedding[VType], Map[VType, VType]] =
      (for {
        embeddingAndFace <- faces
        embedding = embeddingAndFace._1
        cutpoints = embeddingToCutpoints(embedding)
        face = embeddingAndFace._2
      } yield embedding -> findPreviousVertices(face, cutpoints)).toMap

    /* Start at an arbitrary embedding that is only connected to a single cut vertex
     * (the tree structure of a graph where each node represents a biconnected component
     * guarantees that one exists).
     */
    val pendantEntry: (Embedding[VType], Set[VType]) = embeddingToCutpoints.find(embedCont => embedCont._2.size == 1).get
    val cutVertex = pendantEntry._2.head // head is the only element since size == 1

    /* Every embedding merged to far. */
    var result = UndirectedEmbedding[VType]().addEmbedding(pendantEntry._1, Map())

    processedEmbeddings add pendantEntry._1
    processedCutVertices add cutVertex

    /* Keeps track of all embeddings that are adjacent to some part of the current result,
     * as well as which contact vertex that should be used when merging them.
     */
    val notYetProcessedEmbeddings = new mutable.Queue[(Embedding[VType], VType)]()

    /* Kick off the algorithm off by adding the set of connected embeddings to the initial queue. */
    for (embedding <- cutpointsToEmbedding(cutVertex)) {
      if (!processedEmbeddings.contains(embedding))
        notYetProcessedEmbeddings enqueue((embedding, cutVertex))
    }

    /* This is the last embedding to be merged into the final result. Its reference is still needed in order to find
     * the vertex that lies before it on its outer face.
     */
    var previousEmbedding = pendantEntry._1

    /* In order to insert multiple embeddings between two edge adjacencies on a face, a single embedding
     * is marked to be the starting point for the cut vertex. If not, the insertion point would change
     * every time the previous embedding changed.
     */
    var embeddingToUseAsInsertionPoint = Map(cutVertex -> previousEmbedding)

    while (!notYetProcessedEmbeddings.isEmpty) {
      val currentEmbedEntry: (Embedding[VType], VType) = notYetProcessedEmbeddings.dequeue()
      val currentEmbedding = currentEmbedEntry._1
      val unvisitedContacts = embeddingToCutpoints(currentEmbedding) diff processedCutVertices
      processedEmbeddings add currentEmbedding

      /* Add all adjacent embeddings to the queue if they haven't been processed already. */
      for (c <- unvisitedContacts; e <- cutpointsToEmbedding(c)) {
        if (!processedEmbeddings.contains(e)) {
          notYetProcessedEmbeddings enqueue ((e, c))
          processedCutVertices add c
        }
      }

      /* Merge the current embedding at the vertex that precedes the cutpoint on the outer face of
       * the main embedding.
       */
      val cutVertex = currentEmbedEntry._2

      val insertionEmbedding = if (embeddingToUseAsInsertionPoint.contains(cutVertex)) {
        embeddingToUseAsInsertionPoint(cutVertex)
      } else {
        embeddingToUseAsInsertionPoint += (cutVertex -> previousEmbedding)
        previousEmbedding
      }

      val priorVerticesOnOuterface = verticesPriorToCutpoints(insertionEmbedding)
      result = result.addEmbedding(currentEmbedding, Map(cutVertex -> priorVerticesOnOuterface(cutVertex)))
      previousEmbedding = currentEmbedding
    }

    result
  }

  /** Finds a face with vertex amount > 3 */
  private def findOuterFace(embedding: Embedding[VType]): Face[VType] = {
    selectOuterFace(faceComputation.computeFaces(embedding)).getOrElse(
      throw new Error("No unbounded face that matches the supplied function found.")
    )
  }

  /** Maps cutpoints on a face to the vertex that lies after them when traversing the face. */
  private def findPreviousVertices(face: Face[VType], cutpointsOnFace: Set[VType]): Map[VType, VType] = {
    var result = Map[VType, VType]()
    val vertices = face.vertices
    val vs = vertices.toArray
    val length = vs.length
    var i = 0
    while (i < length) {
      if (cutpointsOnFace.contains(vs(i))) {
        val nextVertex = if (i == length - 1) {
          vs(0) // Wrap around to the beginning
        } else {
          vs(i + 1)
        }
        result += (vs(i) -> nextVertex)
      }

      i += 1
    }

    result
  }
}
