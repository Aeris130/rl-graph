package net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.help.{AdjacencyOrdering, BiconnectedComponentMerger}
import net.cyndeline.rlgraph.biconnectivity.{Biconnecter, GraphBiconnection}
import net.cyndeline.rlgraph.connectivity.PlanarConnect
import net.cyndeline.rlgraph.embedding.Vertex
import net.cyndeline.rlgraph.planar.boyerMyrwold.BoyerMyrwoldEmbedder
import net.cyndeline.rlgraph.sorting.DFSOrder
import net.cyndeline.rlgraph.util.SortedGraph

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Biconnects a graph using the algorithm proposed by Goos Kant and Hans L. Bodlaender in the paper
 * "Triangulating Planar Graphs While Minimizing The maximum Degree". The algorithm is a modification
 * of the one proposed by Read, R.C and operates in linear time. The modification guarantees that
 * no vertex in the graph has its degree increased by more than two, and works as follows:
 *
 * An arbitrary embedding of the graph is computed, and its cut vertices are assigned numbers based on their
 * depth first ordering. Every cut vertex is visited in said order, and its immediate neighbors in the embedding
 * are connected using an edge if they belong to different biconnected components. If an edge has previously
 * been added between the current cut vertex and one of its neighbors, that edge is removed.
 *
 * @tparam VType Vertex type in the graph.
 */
class BiconnectivityOperation[VType : TypeTag : ClassTag] extends Biconnecter[VType] {

  /**
   * @param graph A graph.
   * @return A graph with the same vertex set as the input, but with added edges in order to make it biconnected.
   *         Or the same graph if the original was already biconnected.
   */
  override def biconnect(graph: Graph[VType, UnDiEdge]): GraphBiconnection[VType] = {
    val connectivityData = PlanarConnect[VType](graph)
    val connectedGraph = connectivityData.graph
    val componentMerger = new BiconnectedComponentMerger(connectedGraph)
    val cutpointsInGraph = componentMerger.cutPoints.toSet

    if (cutpointsInGraph.isEmpty)
      return GraphBiconnection(connectedGraph, connectivityData.extraEdges)

    var embedding = new BoyerMyrwoldEmbedder[VType]().embed(connectedGraph).get

    /* Perform a depth first search from an arbitrary vertex and order the vertices of the graph in their
     * visitation order.
     */
    val cutpointsInDFSOrder = DFSOrder(connectedGraph, cutpointsInGraph.head)

    /* Keeps track of which vertex pairs has had biconnection edges added between each other. Each Set in the Set
     * contains exatcly two vertices. Sets are used since it doesn't matter which order they were added in.
     */
    var addedEdges = Set[Set[VType]]()

    /* Graph structure that gets modified when biconnecting. */
    var currentGraph = connectedGraph

    /* For every cut vertex in the list, check if any of its consecutive neighbors (any adjacent edges in the embedding)
     * belongs to different biconnected components. If so, add an edge between them.
     *
     * Since the list was generated as the cut vertices were visited during a depth first search, that is the order
     * they will be processed.
     */
    for (cutVertex <- cutpointsInDFSOrder) {

      /* Check every pair of adjacent edges. If two edges has vertices on them that aren't the cut vertex, and whose
       * component sets does not intersect, they belong to different components.
       */
      val adjacencyList = embedding.embeddingFor(cutVertex)
      val adjacencyPairs = new AdjacencyOrdering(adjacencyList.toVector.map(_.adjacentVertex))

      while (adjacencyPairs.hasNext) {
        val nextPair = adjacencyPairs.next()
        val current = nextPair._1
        val next = nextPair._2

        if (!componentMerger.sharesBiconnectedComponent(current, next)) {
          componentMerger.merge(current, next, cutVertex)

          /* Assuming that the edges are embedded clockwise around each vertex, the new edge needs to be embedded
           * att different positions on each neighbor. In the current neighbor, it needs to lie before the edge
           * connecting to the cut vertex. In the next neighbor, it needs to lie after.
           */
          val currentAdjacency = embedding.embeddingFor(current)
          val nextAdjacency = embedding.embeddingFor(next)

          val embeddingBeforeCutvertex = currentAdjacency.entryFor(cutVertex).previous.adjacentVertex
          val embeddingAfterCutVertex = nextAdjacency.entryFor(cutVertex).next.adjacentVertex

          embedding = embedding.embedEdge(Vertex(current) withInsertPosition embeddingAfterCutVertex inVertex next withInsertPosition embeddingBeforeCutvertex)
          currentGraph += current~next
          addedEdges += Set(current, next)

          /* Check if there's an edge added for biconnectivity between either neighbor and the cut vertex.
           * If so, remove it.
           *
           * After deleting the Next vertex in the adjacency pair list, the current pair is removed from
           * it. Otherwise all edges would clump up in a single vertex. Example: The vertices 1, 3, 4, 5 lies
           * around vertex 2. If adding an edge from 1 to 3 causes 3 to be deleted, the next pair would be
           * 1 and 4. Then 4 would be deleted, leading to 1 and 5 etc. All edges would connect at vertex 1,
           * causing its degree to increase by more than 2.
           */
          if (addedEdges.contains(Set(cutVertex, current))) {
            embedding = embedding.deleteEdge(cutVertex, current)
            currentGraph = removeEdge(currentGraph, cutVertex, current)
            adjacencyPairs.delete(current)
          }
          if (addedEdges.contains(Set(cutVertex, next))) {
            embedding = embedding.deleteEdge(cutVertex, next)
            currentGraph = removeEdge(currentGraph, cutVertex, next)
            adjacencyPairs.delete(next)
            if (adjacencyPairs.hasNext) adjacencyPairs.next()
          }
        }
      } // End adjacency iterator loop
    } // End cut vertex loop

    GraphBiconnection(SortedGraph(currentGraph), newEdges(graph, currentGraph) ++ connectivityData.extraEdges)
  }

  private def removeEdge(graph: Graph[VType, UnDiEdge], a: VType, b: VType): Graph[VType, UnDiEdge] = {
    val edge = graph.get(a).edges.find(e => e.contains(b)).getOrElse {
      throw new Error("No edge found between " + a + " and " + b + ".")
    }
    graph - edge
  }

  private def newEdges(oldGraph: Graph[VType, UnDiEdge], newGraph: Graph[VType, UnDiEdge]): Vector[(VType, VType)] = {
    val old = oldGraph.edges.map(e => UnorderedPair[VType](e._1, e._2)).toVector
    val oldAndNew = newGraph.edges.map(e => UnorderedPair[VType](e._1, e._2)).toVector
    oldAndNew.diff(old).map(_.asTuple)
  }

}
