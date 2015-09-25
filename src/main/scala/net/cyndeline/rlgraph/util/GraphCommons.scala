package net.cyndeline.rlgraph.util

import net.cyndeline.rlgraph.embedding.{AdjacencyEntry, Embedding, Vertex}
import net.cyndeline.rlgraph.face.Face

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.{DiEdge, UnDiEdge}
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Common operations performed on the graph-related data structures.
 */
object GraphCommons {

  /**
   * Computes neighbors of a node in a graph in O(n) time, where n = # of neighbors. Uses edges rather than the neighbor
   * method, since it returns nodes in random order when instantiating the same graph multiple times.
   * @param vertex Vertex to retrieve neighbors for.
   * @param graph Graph containing vertex.
   * @tparam V Vertex type used in the graph.
   * @tparam E Edge type used in the graph.
   * @return A list of vertices neighboring the input vertex, or an empty list if no neighbors exist.
   */
  def neighbors[V, E[X] <: EdgeLikeIn[X]](vertex: V, graph: Graph[V, E]): Vector[graph.NodeT] = {
    graph.get(vertex).edges.map(e => if (e._1 == vertex) e._2 else e._1).toVector
  }

  /**
   * Computes the same neighbor set as neighbors(...), but casts the vertex list to their outer objects.
   * @param vertex Vertex to retrieve neighbors for.
   * @param graph Graph containing vertex.
   * @tparam V Vertex type used in the graph.
   * @tparam E Edge type used in the graph.
   * @return A list of vertices neighboring the input vertex, or an empty list if no neighbors exist.
   */
  def outerNeighbors[V, E[X] <: EdgeLikeIn[X]](vertex: V, graph: Graph[V, E]): Vector[V] = {
    val l = neighbors(vertex, graph)
    for {
      n <- l
      outer: V = n
    } yield outer
  }

  /**
   * Computes outgoing neighbors of a node in a graph in O(n) time, where n = # of neighbors. Uses edges() rather than
   * the neighbor method, since it returns nodes in random order when instantiating the same graph multiple times.
   * @param vertex Vertex to retrieve neighbors for.
   * @param graph Graph containing vertex.
   * @tparam V Vertex type used in the graph.
   * @tparam E Edge type used in the graph.
   * @return A list of vertices neighboring the input vertex through an edge going out from the vertex, or an empty
   *         list if no neighbors exist.
   */
  def outgoingOuterNeighbors[V, E[X] <: DiEdge[X]](vertex: V, graph: Graph[V, E]): Vector[V] = {
    graph.get(vertex).edges.filter(_.from == vertex).map(e => e.toOuter.to).toVector
  }

  /**
   * Computes the neighbors of a vertex as well as the edges connecting them.
   * @param vertex Vertex to retrieve neighbors for.
   * @param graph Graph containing vertex.
   * @tparam V Vertex type used in the graph.
   * @tparam E Edge type used in the graph.
   * @return A list of vertices and the edges connecting them to the input vertex, or an empty list if no neighbors
   *         exist.
   */
  def outerEdgeNeighbors[V, E[X] <: EdgeLikeIn[X]](vertex: V, graph: Graph[V, E]): Vector[(V, E[V])] = {
    graph.get(vertex).edges.map(e => {
      val outer: V = if (e._1 == vertex) e._2 else e._1
      (outer, e.toOuter)
    }).toVector
  }

  /**
   * Retrieves the outer vertices of a graph.
   * @param graph Graph structure.
   * @tparam V Vertex type used in the graph.
   * @tparam E Edge type used in the graph.
   * @return A vector containing all vertices in the graph, cast to their outer object.
   */
  def outerVertices[V, E[X] <: EdgeLikeIn[X]](graph: Graph[V, E]): Vector[V] = {
    val vs = for {
      n <- graph.nodes
      outer: V = n
    } yield outer
    vs.toVector
  }

  /**
   * Retrieves the outer edges of a graph.
   * @param graph Graph structure.
   * @tparam V Vertex type used in the graph.
   * @tparam E Edge type used in the graph.
   * @return A vector containing all edges in the graph, cast to their outer object.
   */
  def outerEdges[V, E[X] <: EdgeLikeIn[X]](graph: Graph[V, E]): Vector[E[V]] = graph.edges.toVector.map(e => e.toOuter)

  /**
   * Finds the vertex opposite to a vertex in a graph.
   * @param vertex Vertex to find opposite neighbor of.
   * @param edge Edge that the input vertex is a member of.
   * @tparam V Vertex type.
   * @tparam E Edge type.
   * @return The vertex on the opposite end of the input edge.
   */
  def oppositeVertex[V, E[X] <: EdgeLikeIn[X]](vertex: V, edge: E[V]): V = {
    if (edge._1 == vertex) edge._2
    else if (edge._2 == vertex) edge._1
    else throw new Error("The edge " + edge + " did not contain " + vertex + ", cannot retrieve opposite vertex.")
  }

  /**
   * Splits a list and reconnects it such that the first entry of a specified element ends up as the head of the list.
   * @param e Element to place at the head of the list.
   * @param list List containing the vertex.
   * @tparam E Element type.
   * @return A list with all elements maintaining the same cyclical order as the input, but with the specified
   *         vertex as head.
   */
  def placeElementFirst[E](e: E, list: Vector[E]): Vector[E] = {
    val span: (Vector[E], Vector[E]) = list.span(_ != e)
    span._2 ++ span._1
  }

  /**
   * Embeds an edge inside a face in a planar embedding.
   * @param embedding Embedding of a planar graph.
   * @param face A face computed from the embedding.
   * @param a A vertex to add the new edge to in the embedding.
   * @param b A vertex opposite from 'a that should be connected using the new edge.
   * @return The embedding, containing a new edge from a to b.
   */
  def embedEdgeInFace[V](embedding: Embedding[V], face: Face[V], a: V, b: V): Embedding[V] = {
    val aInsertPos = face.edges.find(_._1 == a).get._2
    val bInsertPos = face.edges.find(_._1 == b).get._2
    embedding.embedEdge(Vertex(a) withInsertPosition bInsertPos inVertex b withInsertPosition aInsertPos)
  }

  /**
   * Splits the disconnected components of a graph into subgraphs.
   * @param graph The graph to compute connected components from.
   * @tparam V Vertex type used in the graph.
   * @tparam E Edge type used in the graph.
   * @return A list of every connected component in the graph. Contains only a single element if the initial graph
   *         was already connected.
   */
  def allConnectedComponents[V : TypeTag, E[X] <: EdgeLikeIn[X] : ({type l[M[_]] = TypeTag[M[V]]})#l](graph: Graph[V, E]): Vector[Graph[V, E]] = {
    val components = graph.componentTraverser()
    val allGraphs = for (disconnectedComponent <- components.toVector) yield {
      val ee = disconnectedComponent.edges.toVector.map(_.toOuter)
      val nn = disconnectedComponent.nodes.toVector.map(n => { val outer: V = n; outer })
      Graph.from(nn, ee)
    }

    allGraphs
  }

  /**
   * Converts the embedding into an undirected graph.
   *
   * @param embedding An embedding to convert into a graph.
   * @return A graph containing all vertices in the embedding, with edges constructed by the factory according to
   *         the embedded edge entries.
   */
  def embeddingAsGraph[VType : TypeTag](embedding: Embedding[VType]): Graph[VType, UnDiEdge] = {
    val edges = new ArrayBuffer[UnDiEdge[VType]]()
    val vertices = embedding.embeddedVertices.iterator

    while (vertices.hasNext) {
      val vertex = vertices.next()
      val adjacencyEntries: Iterator[AdjacencyEntry[VType]] = embedding.embeddingFor(vertex).iterator

      while (adjacencyEntries.hasNext) {
        val entry = adjacencyEntries.next()
        edges += vertex~entry.adjacentVertex
      }
    }

    Graph.from(embedding.embeddedVertices, edges.toVector)
  }

  /**
   * @param start The first edge to traverse, connected to the new path.
   * @param block The node in the first edge that the algorithm cannot traverse past without going into other areas.
   * @return The sub-topology that can be reached without passing the block.
   */
  def computeSubTopology[V : TypeTag, E[X] <: EdgeLikeIn[X] : ({type l[M[_]] = TypeTag[M[V]]})#l](start: E[V], block: V, graph: Graph[V, E]): Graph[V, E] = {
    val other = if (start._1 == block) start._2 else start._1
    val sub = graph.get(other).withSubgraph(nodes = _ != block).toGraph
    val startEdge = graph.get(other).edges.find(_.contains(block)).get
    Graph(startEdge) ++ sub // Needed since sub uses another Graph trait.
  }

  /**
   * Traverses a graph from a starting point and returns the sub graph that was traversed.
   * @param graph Graph to traverse.
   * @param start Vertex to start traversing from.
   * @param include Returns true if an edge should be traversed, otherwise false.
   * @tparam V Vertex type in the graph.
   * @tparam E Edge type in the graph.
   * @return The sub graph that was traversed from the starting point, passing only the edges that were accepted
   *         by the user.
   */
  def traversedSubGraph[V, E[X] <: EdgeLikeIn[X]: ({type l[M[_]] = TypeTag[M[V]]})#l](graph: Graph[V, E], start: V, include: E[V] => Boolean): Graph[V, E] = {
    val currentEdges = new ListBuffer[graph.EdgeT]()
    val visited = new mutable.HashSet[graph.EdgeT]()
    val neighbors = new mutable.Stack[graph.NodeT]()
    neighbors push graph.get(start)

    while (!neighbors.isEmpty) {
      val current = neighbors.pop()
      val neighborEdges = current.outgoing.filter(e => include(e.toOuter) && !visited.contains(e))
      visited ++= neighborEdges
      currentEdges ++= neighborEdges
      neighbors.pushAll(neighborEdges.map(e => e.find(_ != current).get))
    }

    Graph.from[V, E](List(start), currentEdges.toVector.map(_.toOuter))
  }

}
