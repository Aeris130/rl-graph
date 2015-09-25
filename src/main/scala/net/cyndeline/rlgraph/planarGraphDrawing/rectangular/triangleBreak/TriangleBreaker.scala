package net.cyndeline.rlgraph.planarGraphDrawing.rectangular.triangleBreak

import net.cyndeline.rlgraph.embedding.{Embedding, Vertex}
import net.cyndeline.rlgraph.face.FaceComputation
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.dataStructure.RVertex
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.triangleBreak.help.BubbleGraph
import net.cyndeline.rlgraph.subgraph.triangles.TriangleFinder
import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
 * Modifies a planar embedding by breaking every separating triangle in it.
 */
class TriangleBreaker {
  private val miniMaxPruner = new PruneMiniMax()

  /**
   * @param embedding An embedding to split all separating triangles in.
   * @tparam VType Vertex type in the embedding.
   */
  def breakTriangles[VType : TypeTag : ClassTag](embedding: Embedding[RVertex[VType]]): Embedding[RVertex[VType]] = {
    var currentEmbedding = embedding
    val faces = new FaceComputation[RVertex[VType]]().computeFaces(embedding)
    val triangles = new TriangleFinder().computeTriangles(GraphCommons.embeddingAsGraph(embedding))
    val separatingTriangles = new SeparatingTriangleFilter(triangles, faces.filter(_.vertexSize == 3)).triangles
    val bubbleGraphs = BubbleGraph(separatingTriangles)
    val edgesToBeSplit = new ListBuffer[(RVertex[VType], RVertex[VType])]()

    for (bg <- bubbleGraphs) {
      val edges = miniMaxPruner.applyHeuristic(bg)
      edgesToBeSplit ++= edges.map(bubbleVertex => (bubbleVertex._1, bubbleVertex._2))
    }

    var splitId = 0
    for (edgeToSplit <- edgesToBeSplit) {
      currentEmbedding = splitEdge(edgeToSplit._1, edgeToSplit._2, currentEmbedding, splitId)
      splitId += 1
    }

    currentEmbedding
  }

  // Deletes an edge and inserts the split after the counter clockwise entries of the old edge in both vertices.
  private def splitEdge[VType](from: RVertex[VType],
                               to: RVertex[VType],
                               embedding: Embedding[RVertex[VType]],
                               splitKey: Int): Embedding[RVertex[VType]] = {
    var emb = embedding
    val fromAdjacency = emb.embeddingFor(from).entryFor(to)
    val toAdjacency = emb.embeddingFor(to).entryFor(from)
    val fromCC = fromAdjacency.previous.adjacentVertex
    val toCC = toAdjacency.previous.adjacentVertex
    emb = emb.deleteEdge(from, to)
    val newVertex = RVertex.split[VType]("Split(" + splitKey.toString + ")")
    emb
      .embedEdge(Vertex(from) withDefaultPositionInVertex newVertex withInsertPosition fromCC)
      .embedEdge(Vertex(to) withDefaultPositionInVertex newVertex withInsertPosition toCC)
  }
}
