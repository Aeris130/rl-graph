package net.cyndeline.rlgraph.pathfinding

import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge
import scalax.collection.immutable.Graph

/**
 * Computes the longest path from one vertex in a graph to the other vertices. As the longest-path problem is NP-hard
 * for general graphs, this class is restricted to directed acyclic graphs.
 *
 * @param vertex The vertex to compute distances from. Will receive distance 0 in every path.
 * @param graph A connected, directed and acyclic graph, with weights used to represent the distance from one vertex
 *              to the next.
 */
class LongestDAGPath[VType : TypeTag] private (vertex: VType, graph: Graph[VType, WDiEdge]) {
  require(!graph.isEmpty, "Cannot compute longest paths for an empty graph.")
  require(graph.contains(vertex), "The vertex " + vertex + " was not found in the graph " + graph)

  // Id is used to sort edges in a unique order
  private case class LabelVertex(v: VType, id: Int) {
    var distance: Int = 0

    override def toString: String = "[" + v + " id: " + id + ", D: " + distance + "]"
  }

  /** Assigns distances to every labeled vertex in the distance graph. */
  private val distanceLabels: Graph[LongestDAGPath.this.type#LabelVertex, WDiEdge] = {
    val topologicalSort = new TopologicalDAGSort[LabelVertex]()
    val labeledVertices = labelGraph
    val processingOrder = topologicalSort.order(unWeighted(labeledVertices)).toIterator

    /* For each vertex in the topological order, set distance(n) in its neighbors
     * to max(distance(n), distance(v) + weight(v->n)).
     */
    while (processingOrder.hasNext) {
      val next = processingOrder.next()
      val neighbors = labeledVertices.get(next)
        .outgoing
        .toVector
        .sortBy(e => GraphCommons.oppositeVertex(next, e.toOuter).id)
        .iterator

      while (neighbors.hasNext) {
        val nextNeighborEdge = neighbors.next()
        val neighbor = GraphCommons.oppositeVertex(next, nextNeighborEdge.toOuter)
        neighbor.distance = Math.max(neighbor.distance, next.distance + nextNeighborEdge.weight).toInt
      }
    }

    labeledVertices
  }

  // Speeds up searches by not having to fetch it every time.
  private val stopLabel = findLabeledVertex(vertex)

  /**
   * Computes the numeric distance from the initial vertex to a specified vertex v. The distance is the cost accrued
   * when traversing the weighted edges along the path (if no weights were specified, every edge will use the weight 1).
   * @param v Vertex t compute longest path distance to.
   * @return The cost of traversing the edges along the path from start to v.
   */
  def distanceTo(v: VType): Int = {
    val p = criticalPath(v)
    require(!p.isEmpty, "No path to " + v + " found in graph, cannot compute distance.")
    p.last.distance
  }

  /**
   * Computes the longest path from the vertex used to construct this object with to a specified vertex.
   * @param v Vertex t compute longest path to.
   * @return The list of all vertices along the longest path from start to the specified vertex v.
   */
  def pathTo(v: VType): Option[Path[VType, WDiEdge]] = {
    val p = criticalPath(v).map(_.v)

    if (p.isEmpty) {
      None
    } else {
      val start = p.head
      val vPairs = p.zip(p.drop(1))
      val edges = vPairs.map(pair => {
        val edge = graph.get(pair._1).findOutgoingTo(graph.get(pair._2)).getOrElse(throw new Error("Path found that lacks a corresponding outgoing edge."))
        edge.toOuter
      })

      if (!edges.isEmpty)
        Some(Path(start, edges))
      else
        Some(Path[VType, WDiEdge](start))
    }
  }

  private def criticalPath(from: VType): Vector[LabelVertex] = {
    val start = findLabeledVertex(from)
    val stop = stopLabel
    val path = new ListBuffer[LabelVertex]()
    path += start

    var current = start
    while (current != stop) {
      val incomingNeighbors = distanceLabels.get(current)
        .incoming
        .toVector
        .sortBy(e => GraphCommons.oppositeVertex(current, e.toOuter).id) // Needed in case multiple neighbors has the same distance

      if (incomingNeighbors.isEmpty)
        return Vector() // No path available

      /* The weight of the edge going from the neighbor to the current vertex must also be taken into consideration,
       * otherwise the edge leading into any given forking in the graph will not affect the choice of neighbor to
       * select.
       *
       * If the maximum distance is 0, that means the stop vertex is adjacent (possibly along with other vertices
       * having in-degree 0).
       */
      val highestNeighborEdge = incomingNeighbors.maxBy(e => GraphCommons.oppositeVertex(current, e.toOuter).distance + e.weight)
      val highestNeighbor = GraphCommons.oppositeVertex(current, highestNeighborEdge.toOuter)

      if (highestNeighbor.distance == 0 && incomingNeighbors.exists(e => e.from == stop)) {
        path += stop
        current = stop
      } else {
        path += highestNeighbor
        current = highestNeighbor
      }
    }

    path.toVector.reverse
  }

  private def findLabeledVertex(v: VType): LabelVertex = distanceLabels.nodes.find(_.v == v).getOrElse {
    throw new NoSuchElementException("The vertex " + v + " was not present in the DAG.")
  }

  /** Wraps every vertex in the weighted graph in a LabelVertex. */
  private def labelGraph: Graph[LabelVertex, WDiEdge] = {
    val labeledEdges = new ListBuffer[WDiEdge[LabelVertex]]()
    var nextId = 0
    val vMap = new mutable.HashMap[VType, LabelVertex]()

    // Add the initial vertex manually, in case the graph only has a single vertex
    val initialLabel = LabelVertex(vertex, nextId)
    nextId += 1
    vMap += vertex -> initialLabel

    for (e <- graph.edges) {
      val outer = e.toOuter
      val from = vMap.get(outer.from).getOrElse {
        nextId += 1
        val l = LabelVertex(outer.from, nextId)
        vMap += outer.from -> l
        l
      }
      val to = vMap.get(outer.to).getOrElse {
        nextId += 1
        val l = LabelVertex(outer.to, nextId)
        vMap += outer.to -> l
        l
      }
      labeledEdges += ((from~>to) % e.weight)
    }

    Graph.from(vMap.values.toVector, labeledEdges.toVector)
  }

  /** Converts a weighted graph with labeled vertices into an unweighted one. Used for topological sorting. */
  private def unWeighted(g: Graph[LabelVertex, WDiEdge]): Graph[LabelVertex, DiEdge] = {
    val undirectedEdges = new ListBuffer[DiEdge[LabelVertex]]()
    for (e <- g.edges) {
      val outer = e.toOuter
      undirectedEdges += outer.from~>outer.to
    }

    val vertices = g.nodes.map(n => {
      val outer: LabelVertex = n
      outer
    }).toVector

    Graph.from(vertices, undirectedEdges.toVector)
  }
}

/**
 * Factory object that allows paths to be computed on both weighted and unweighted graphs.
 */
object LongestDAGPath {

  /**
   * @param vertex The vertex to compute distances from. Will receive distance 0 in every path.
   * @param graph A connected, directed and acyclic graph, with weights used to represent the distance from one vertex
   *              to the next.
   */
  def withWeights[VType : TypeTag](vertex: VType, graph: Graph[VType, WDiEdge]): LongestDAGPath[VType] = new LongestDAGPath[VType](vertex, graph)

  /**
   * @param vertex The vertex to compute distances from. Will receive distance 0 in every path.
   * @param graph A connected, directed and acyclic graph.
   */
  def withoutWeights[VType : TypeTag](vertex: VType, graph: Graph[VType, DiEdge]): LongestDAGPath[VType] = new LongestDAGPath[VType](vertex, weighted(graph))

  /** Converts an unweighted unlabeled graph into a weighted one with every weight set to 1. */
  private def weighted[VType : TypeTag](g: Graph[VType, DiEdge]): Graph[VType, WDiEdge] = {
    val weighted = new ListBuffer[WDiEdge[VType]]()
    for (e <- g.edges) {
      val outer = e.toOuter
      weighted += outer.from~>outer.to % 1
    }

    val vertices = g.nodes.map(n => {
      val outer: VType = n
      outer
    }).toVector

    Graph.from(vertices, weighted.toVector)
  }

}
