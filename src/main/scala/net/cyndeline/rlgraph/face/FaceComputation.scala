package net.cyndeline.rlgraph.face

import net.cyndeline.rlgraph.embedding.{AdjacencyList, Embedding}
import net.cyndeline.rlgraph.planar.ComputeFaceOperation
import net.cyndeline.rlgraph.planar.demoucron.operation.DemoucronEmbedding

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.{Graph => ImmutableGraph}
import scalax.collection.mutable.Graph

/**
 * Creates a directed graph for each edge in an embedding, with two directed edges going both ways
 * between every pair of vertices. The algorithm then selects one edge at random, and traverses the embedding
 * in a counter clockwise direction until it reaches the original edge. Every edge traversed this way
 * is part of a face.
 *
 * @constructor Creates a new face computation object.
 * @tparam VType The type of nodes used in each face.
 */
class FaceComputation[VType : TypeTag : ClassTag] extends ComputeFaceOperation[VType] {

  /**
   * Compute the faces of a graph.
   *
   * @param embedding A planar embedding of a graph connected graph, where edges are embedded in a clockwise
   *                  order around each vertex.
   * @return A list of faces, where each face is represented by a list of vertices in the order they are
   *         traversed around the face. The last entry in the face is implied to be connected to the first.
   */
  def computeFaces(embedding: Embedding[VType]): Vector[Face[VType]] = {
    val graph = Graph[VType, DiEdge]()
    val embeddedValues: Iterator[(VType, AdjacencyList[VType])] = embedding.iterator

    /*
     * STEP 1: Setup a graph representation of the embedding, with two directed edges for each edge
     * in the embedding. Duplicates will be filtered out by the insertion.
     */
    while (embeddedValues.hasNext) {
      val vertexAdjacencyList: (VType, AdjacencyList[VType]) = embeddedValues.next()
      val vertex = vertexAdjacencyList._1
      val adjacencyList = vertexAdjacencyList._2
      val entries = adjacencyList.iterator

      while(entries.hasNext) {
        val entry = entries.next()
        graph += (vertex~>entry.adjacentVertex)
        graph += (entry.adjacentVertex~>vertex)
      }
    }

    /*
     * STEP 2: Traverse and delete edges until the graph is empty. Traversal order is selected using the embedding.
     */
    val faces = new ArrayBuffer[Face[VType]]
    while (graph.edges.nonEmpty) {
      val randomEdge: DiEdge[VType] = graph.edges.toOuter.head
      val to = randomEdge.to
      val from = randomEdge.from
      val face = new ArrayBuffer[VType]()

      /* The traversal starts at the edge incident to the random edge, to keep the start-point in the graph. */
      var faceEdge = nextEdge(from, to, embedding, graph)

      while(faceEdge != randomEdge) {
        val fTo = faceEdge.to
        val fFrom = faceEdge.from
        face += fFrom
        graph -= faceEdge
        faceEdge = nextEdge(fFrom, fTo, embedding, graph)
      }

      graph -= randomEdge
      face += from
      faces += new Face(face.toVector)
      face.clear()
    }

    faces.toVector
  }

  /**
   * Embeds the graph and computes the faces using the embedding.
   * @param graph Graph to compute faces from.
   * @return A list of faces, where each face is represented by a list of vertices in the order they are
   *         traversed around the face. The last entry in the face is implied to be connected to the first.
   */
  def computeFacesFromGraph(graph: ImmutableGraph[VType, UnDiEdge]): Vector[Face[VType]] = {
    val embedder = new DemoucronEmbedding[VType, UnDiEdge]()
    computeFaces(embedder.embed(graph).getOrElse(throw new IllegalArgumentException("The graph " + graph + " was not planar.")))
  }

  /**
   * @param edgeStart A vertex in an edge.
   * @param edgeStop The vertex opposite of the edge start.
   * @param embedding An embedding containing the edge.
   * @return The face that results from traversing the embeddings adjacencies counter clockwise, beginning in the entry
   *         of edgeStop when moving from edgeStart.
   */
  def computeSingleFace(edgeStart: VType, edgeStop: VType, embedding: Embedding[VType]): Face[VType] = {
    var currentEntry = embedding.embeddingFor(edgeStart).entryFor(edgeStop).moveTo
    val face = new ListBuffer[VType]()

    do {
      face += currentEntry.adjacentVertex
      currentEntry = currentEntry.previous.moveTo

    } while (currentEntry.adjacentVertex != edgeStart)

    new Face(face.toVector)
  }

  private def nextEdge(from: VType, to: VType, embedding: Embedding[VType], graph: Graph[VType, DiEdge]): DiEdge[VType] = {
    val toEmbedding = embedding.embeddingFor(to)
    val fromEntry = toEmbedding.entryFor(from)

    var current = fromEntry.previous
    do {
      val edge = to~>current.adjacentVertex
      if (graph.contains(edge)) {
        return edge
      } else {
        current = current.previous
      }

    } while (current.adjacentVertex != fromEntry.adjacentVertex)

    throw new IllegalArgumentException("The graph " + graph + " contained no edge going from " + to + " to another edge in the embedding " + toEmbedding)
  }
}
