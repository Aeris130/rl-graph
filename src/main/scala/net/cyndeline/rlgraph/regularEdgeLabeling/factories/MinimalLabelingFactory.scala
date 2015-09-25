package net.cyndeline.rlgraph.regularEdgeLabeling.factories

import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.regularEdgeLabeling.factories.help.{OuterCycle, OuterEdgeAppender}
import net.cyndeline.rlgraph.regularEdgeLabeling.{EdgeLabeling, RELFactory, RegularEdgeLabeling}
import net.cyndeline.rlgraph.util.{GraphCommons, IndexGraph, IndexVertex => IV}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Uses the iterative algorithm by Eric Fusy, described in "Transversal structures on triangulations, with application
 * to straight-line drawing", to compute a unique minimal REL. The minimal REL is defined as an edge partition between
 * T1 and T2 where every essential alternating 4-cycle is left-alternating.
 */
class MinimalLabelingFactory extends RELFactory {

  def produceRegularEdgeLabeling[V: TypeTag : ClassTag](embedding: Embedding[V], north: V, south: V, west: V, east: V): EdgeLabeling[V] = {
    require(embedding.embeddingFor(north).entryFor(west).next.adjacentVertex == east, "The embedding must store the neighbors of North in the clockwise order east -> west.")
    val outerEdgeAppender = new OuterEdgeAppender[V](north, south, west, east)
    val tempT1 = new ListBuffer[(V, V)]()
    val tempT2 = new ListBuffer[(V, V)]()

    val outer = Set(north, south, west, east)

    /* Initially, edges between north and its inner children are directed to north and added to T1. */
    for (inner <- embedding.neighborsOf(north) if !outer.contains(inner)) {
      tempT1 += ((inner, north))
    }
    for (n <- embedding.neighborsOf(south) if n != west && n != east) {
      tempT1 += south -> n
    }

    var currentEmbedding = indexedVertices(embedding)
    val indexNorth = currentEmbedding.embeddedVertices.find(_.valueOfIndex == north).get
    val indexWest = currentEmbedding.embeddedVertices.find(_.valueOfIndex == west).get
    val indexEast = currentEmbedding.embeddedVertices.find(_.valueOfIndex == east).get
    val indexSouth = currentEmbedding.embeddedVertices.find(_.valueOfIndex == south).get
    var cycle = new OuterCycle(findRightToLeftPath(currentEmbedding, indexNorth, indexWest, indexEast).reverse, currentEmbedding.vertexSize)

    var pointer = cycle.vertices.length - 1 // Starts at the rightmost vertex
    val stack = new mutable.Stack[MatchingPath[V]]()
    var visitedVertices = Vector.fill(currentEmbedding.embeddedVertices.size)(false)
    var done = false

    // The northern vertex has been processed, and can be removed.
    currentEmbedding = currentEmbedding.deleteVertex(indexNorth)

    while (!done) {

      if (!stack.isEmpty && stack.head.leftmost == cycle.vertices(pointer)) {
        val path = stack.pop()

        for (innerEdge <- edgesBetweenPathAndCycle(path, cycle, currentEmbedding)) {
          tempT1 += innerEdge._1 -> innerEdge._2
        }

        val oldCycleVertices = cycle.interval(path.leftmost, path.rightmost)
        addPathToT2(oldCycleVertices, tempT2)

        cycle = cycle.addMatchingPath(path.leftmost, path.rightmost, path.leftToRightPath)
        pointer = cycle.positionOf(path.rightmost)

        /* Remove the vertices that no longer are members of the cycle due to the new path, to avoid having to
         * take them into consideration when looking for neighbors in the embedding.
         */
        for (v <- oldCycleVertices.drop(1).dropRight(1)) {
          currentEmbedding = currentEmbedding.deleteVertex(v)
        }

      } else {
        val v = cycle.vertices(pointer)
        val w = cycle.vertices(pointer - 1)
        val newMatchingPath = findMatchingPath(v, w, cycle, currentEmbedding, indexSouth, visitedVertices)

        if (newMatchingPath.isDefined) {
          val path = newMatchingPath.get._1
          visitedVertices = newMatchingPath.get._2
          stack.push(path)

        } else if (stack.isEmpty && w == indexWest) { // Done when no matching path is found AND we're at the end of C.
          done = true
        }

        pointer -= 1
      }
    }

    // The algorithm stops when the cycle contains the neighbors of South, and needs to have them added manually.
    addPathToT2(cycle.vertices, tempT2)

    val t1Edges = tempT1.toVector
    val t2Edges = tempT2.toVector
    val g1 = outerEdgeAppender.addExtraG1Edge(outerEdgeAppender.trimEmbedding(t1Edges, embedding))
    val g2 = outerEdgeAppender.addExtraG2Edge(outerEdgeAppender.trimEmbedding(t2Edges, embedding))

    RegularEdgeLabeling(t1Edges, t2Edges, embedding, g1, g2, north, west, south, east)
  }

  /**
   * @param v The rightmost vertex pointed at.
   * @param w The vertex left on v on C.
   * @param cycle The current cycle C.
   * @param embedding Remaining embedding, containing only C and vertices that has not yet been members of C.
   * @return A matching path from v to v', starting at v' and ending at v (i.e moves counter clockwise around w).
   *         None if the path only contains the south vertex.
   */
  private def findMatchingPath[V](v: IV[V], w: IV[V],
                                  cycle: OuterCycle[V],
                                  embedding: Embedding[IV[V]],
                                  south: IV[V],
                                  visited: Vector[Boolean]): Option[(MatchingPath[V],Vector[Boolean])] = {

    /* A valid path can only be found if v and w shares at least one neighbor that isn't South. */
    if (embedding.neighborsOf(v).filter(n => !cycle.contains(n) && n != w && n != south).isEmpty)
      return None

    /* The neighbors of w in clockwise order, starting at (but not including) v, and ending at the cycle-vertex cv
     * after w. This can easily be computed by cycling the neighbors so that v lies at the head of the list (implying
     * that cv lies last) since any old vertex is removed from the embedding. Thus the only neighbors of w are its
     * neighbors on C, as well as unprocessed vertices.
     */
    val neighbors = GraphCommons.placeElementFirst(v, embedding.neighborsOf(w).filter(_ != south)).drop(1).dropRight(1)

    if (neighbors.isEmpty)
      return None
    else if (neighbors.exists(n => visited(n.index)))
      return None

    /* By iterating over the neighbor vertices, the vertex closest to v that connects to a neighbor on C left of w
     * can be found. As the embedding is triangular, such a vertex is guaranteed to exist.
     */
    val positionOfW = cycle.positionOf(w)
    val ctv = neighbors
      .find(v => embedding.neighborsOf(v).exists(n => cycle.contains(n) && cycle.positionOf(n) < positionOfW)).get

    /* Select the neighbor of ctv on C that lies furthest to the left. */
    val leftmost = embedding.neighborsOf(ctv).filter(cycle.contains).minBy(cycle.positionOf)

    /* Only the clockwise neighbors up until the one closest to v should be kept for the path. */
    val neighborsOnPath = neighbors.splitAt(neighbors.indexOf(ctv) + 1)._1.reverse
    val updatedVisited = visitPath(neighborsOnPath, visited)

    Some((MatchingPath(leftmost, neighborsOnPath, v), updatedVisited))
  }

  /**
   * @return Tuples where the first vertex is a vertex on the path, and the second is a vertex on the cycle if the
   *         submitted embedding has an edge between the two.
   */
  private def edgesBetweenPathAndCycle[V](path: MatchingPath[V], cycle: OuterCycle[V], embedding: Embedding[IV[V]]): Vector[(V, V)] = {
    val edges = new ListBuffer[(V, V)]()
    for (v <- path.leftToRightPath; n <- embedding.neighborsOf(v) if n != path.leftmost && n != path.rightmost) {
      if (cycle.contains(n))
        edges += (v.valueOfIndex -> n.valueOfIndex)
    }
    edges.toVector
  }

  /**
   * @return The vector containing visitation statuses for every vertex, and a boolean: If true, an already visited
   * inner vertex was found. Otherwise false.
   */
  private def visitPath[V](path: Vector[IV[V]], visited: Vector[Boolean]): Vector[Boolean] = {
    var v = visited
    for (innerVertex <- path) {
      require(!visited(innerVertex.index))
      v = v.updated(innerVertex.index, true)
    }
    v
  }

  private def addPathToT2[V](path: Vector[IV[V]], t2Edges: ListBuffer[(V, V)]) {
    val pathEdges = path zip path.drop(1)
    for (e <- pathEdges) {
      t2Edges += e._1.valueOfIndex -> e._2.valueOfIndex
    }
  }

  // Neighbors of north, ordered right -> left.
  private def findRightToLeftPath[V](embedding: Embedding[IV[V]], north: IV[V], west: IV[V], east: IV[V]): Vector[IV[V]] = {
    val westEntry = embedding.embeddingFor(north).entryFor(west)
    val rightmostVertex = if (westEntry.next.adjacentVertex == east) east else west
    GraphCommons.placeElementFirst(rightmostVertex, embedding.neighborsOf(north))
  }

  private def indexedVertices[V : TypeTag](embedding: Embedding[V]): Embedding[IV[V]] = {
    val indexVertices: Graph[IV[V], UnDiEdge] = IndexGraph.undirected(GraphCommons.embeddingAsGraph(embedding))
    val vertexToIndex = new mutable.HashMap[V, IV[V]]()
    for (n <- indexVertices.nodes)
      vertexToIndex += n.valueOfIndex -> n

    embedding.map((v: V) => vertexToIndex(v))
  }

  private case class MatchingPath[V](leftmost: IV[V], leftToRightPath: Vector[IV[V]], rightmost: IV[V])

}
