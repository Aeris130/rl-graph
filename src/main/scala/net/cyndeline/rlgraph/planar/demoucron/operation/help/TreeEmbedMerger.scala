package net.cyndeline.rlgraph.planar.demoucron.operation.help

import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.embedding.{AdjacencyEntry, Embedding}

import scala.collection.mutable
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Merges a set of embeddings computed from a set of biconnected connected components.
 * These make up a BC tree, as such the embeddings can be merged with any other embedding
 * connected to one of its cut vertices, as the tree-like structure guarantees that no other
 * vertex is shared between the two embeddings, or any other embeddings connected to them.
 *
 * @constructor Constructs a new tree-embedding merger.
 */
class TreeEmbedMerger[VType : Manifest, EType[X] <: UnDiEdge[X]] {

  /**
   * Merges a set of embeddings that shares at most one vertex with any other given embedding.
   * All components are merged such that the edges around a cut vertex between any two components
   * remain adjacent to each other based on the component they belong to.
   *
   * @param embeddings All embeddings to be merged, mapped to the graph component they were created from.
   *                   The union of all embeddings must be a BC tree-like structure with respect to vertices
   *                   connecting the components.
   * @return A single embedding containing all edges embedded in the supplied map.
   */
  def merge(embeddings: Map[Embedding[VType], Graph[VType, EType]]): Embedding[VType] = {
    if (embeddings.size == 1) return embeddings.head._1
    else if (embeddings.isEmpty) throw new IllegalArgumentException("No embeddings specified in merge")

    val mappings = mapEmbeddingsAndCutVertices(embeddings)
    val processedCutVertices = new mutable.HashSet[VType]()
    val processedEmbeddings = new mutable.HashSet[Embedding[VType]]()

    /* Keeps track of all embeddings that are adjacent to some part of the current result,
     * as well as which contact vertex that should be used when merging them.
     */
    val notYetProcessedEmbeddings = new mutable.Queue[(Embedding[VType], VType)]()

    /* Every embedding merged to far. */
    var result: Embedding[VType] = UndirectedEmbedding[VType]()

    val contactVerticesToEmbedding: Map[VType, Set[Embedding[VType]]] = mappings._1
    val embeddingToContact: Map[Embedding[VType], Set[VType]] = mappings._2

    /* Start at an arbitrary embedding that is only connected to a single cut vertex (the
     * tree structure guarantees that one exists). This is needed since an empty embedding
     * can't be merged (needs to specify one or more vertices to merge at).
     */
    val pendantEntry = embeddingToContact.find(embedCont => embedCont._2.size == 1).get
    val cutVertex = pendantEntry._2.head // head is the only element since size == 1
    result = pendantEntry._1
    processedEmbeddings add result
    processedCutVertices add cutVertex

    /* Kick off the algorithm off by adding the set of connected embeddings to the initial queue. */
    for (unprocessedEmbedding <- contactVerticesToEmbedding(cutVertex)) {
      if (!processedEmbeddings.contains(unprocessedEmbedding)) notYetProcessedEmbeddings enqueue((unprocessedEmbedding, cutVertex))
    }

    while (!notYetProcessedEmbeddings.isEmpty) {
      val currentEmbedEntry: (Embedding[VType], VType) = notYetProcessedEmbeddings.dequeue()
      val currentEmbedding = currentEmbedEntry._1
      val unvisitedContacts = embeddingToContact(currentEmbedding) diff processedCutVertices
      processedEmbeddings add currentEmbedding

      /* Add all adjacent embeddings to the queue if they haven't been processed already. */
      for (c <- unvisitedContacts; e <- contactVerticesToEmbedding(c)) {
        if (!processedEmbeddings.contains(e)) {
          notYetProcessedEmbeddings enqueue ((e, c))
          processedCutVertices add c
        }
      }

      /* Merge the current embedding at the end of the already present result. */
      val cutVertex = currentEmbedEntry._2
      val lastEntry: AdjacencyEntry[VType] = result.embeddingFor(cutVertex).head.previous
      result = result.addEmbedding(currentEmbedding, Map(cutVertex -> lastEntry.adjacentVertex))
    }

    result
  }

  /**
   * Maps every contact vertex to its connected embeddings, and every embedding to its contact vertices.
   */
  private def mapEmbeddingsAndCutVertices(embeddings: Map[Embedding[VType], Graph[VType, EType]]): (Map[VType, Set[Embedding[VType]]], Map[Embedding[VType], Set[VType]]) = {
    var contactVerticesToEmbed: Map[VType, Set[Embedding[VType]]] = Map()
    var embeddingToContact = Map[Embedding[VType], Set[VType]]()

    /* Map embeddings and contact vertices towards one another. */
    for (embAndGraph <- embeddings.iterator) {
      val embedding = embAndGraph._1
      val graph = embAndGraph._2
      var contacts = Set[VType]()

      for (other <- embeddings.values) {
        if (other != graph) {
          val newContactVertices = (graph intersect other).nodes.toOuter.toSet[VType]

          if (!newContactVertices.isEmpty) {
            contacts ++= newContactVertices

            for (c <- newContactVertices) {
              val currentEmbeddings = contactVerticesToEmbed.getOrElse(c, Set[Embedding[VType]]())
              contactVerticesToEmbed += (c -> (currentEmbeddings + embedding))
            }

          }
        }
      }

      embeddingToContact += (embAndGraph._1 -> contacts)
    }

    (contactVerticesToEmbed, embeddingToContact)
  }
}
