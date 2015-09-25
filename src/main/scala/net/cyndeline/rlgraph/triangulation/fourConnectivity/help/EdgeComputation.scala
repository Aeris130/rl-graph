package net.cyndeline.rlgraph.triangulation.fourConnectivity.help

import net.cyndeline.rlcommon.util.DoubleLinkedList
import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Methods used when augmenting a face with additional edges.
 */
object EdgeComputation {

  /**
   * u1 and u(p) has a common neighbor w, and a vertex u(j) has been selected as the minimal j > 1 that does not have
   * w as a neighbor. That u(j) turned out to be v. w is present in the face.
   *
   * Edges will be embedding into the current face between v and u2 -> u(p-1). If v and w share a common neighbor before
   * any edges are added, then no edge will go from v to w. Instead the face [v, neighbor_before_w, w, neighbor_after_w]
   * will be returned. Otherwise, an edge v to w is added, making the entire face triangular.
   *
   * @return The face that remains if w and v shares a common neighbor.
   */
  def ujEqualsV[V : TypeTag : ClassTag](v: V, u1: V, uP: V, w: V, face: Face[V], addEdgeFromVToW: Boolean): Augmentation[V] = {
    val edgesToAdd = new ListBuffer[(V, V)]()
    val reverse = face.vertexPresentAfter(v) == uP
    val start = if (reverse) uP else u1
    val stop = if (reverse) u1 else uP
    val u2First = GraphCommons.placeElementFirst(face.vertexPresentAfter(start), face.vertices)
    val shouldGetEdges = u2First.span(_ != stop)._1 // Contains every vertex between and including u2 and u(p - 1).
    var latestInsert = start

    for (vertex <- shouldGetEdges) {
      if (vertex != w || addEdgeFromVToW) {
        edgesToAdd += ((v, vertex))
        latestInsert = vertex
      }
    }

    Augmentation(edgesToAdd.toVector)
  }

  /**
   * u(p) > u(j) > u1.
   *
   * Add edges from u(j) to u1 ... u(j - 2). This part does not correspond to the changes made in OCoRDs documentation,
   * however, looking at the source code shows that the edges are added in the same way as the original article.
   * Perhaps a typo? The documentation also does not mention that if one of these vertices is w, no such edge is added
   * if j and w shares a common neighbor.
   *
   * If u(j) == u(p - 1) or if the edge (u(j), u(p)) doesn't exist in the graph, add the edge (v, u(j)).
   *
   * Otherwise, find the maximal k < p such that u(k) isn't adjacent to u(j). u(j) itself is a candidate for this as
   * u(j) doesn't have any edges to u(j).
   * If u(k) == u(j), then change it to u(j + 1).
   * Add the edge u(k) -> u1.
   * Add edges from v to u(k) ... u(p - 1).
   *
   * @return The list of faces with size > 3 that results from adding edges to the specified face.
   */
  def maximalK[V : TypeTag : ClassTag](v: V, u1: V, uP: V, uJ: V, w: V, face: Face[V], embedding: Embedding[V]): Augmentation[V] = {
    require(uJ != v && uJ != u1 && uJ != uP, "Required: " + u1 + " > " + uJ + " > " + uP + ", currently " + uJ)
    val reverse = face.vertexPresentAfter(v) == uP
    val dll = if (reverse) new DoubleLinkedList(face.vertices.reverse:_*) else new DoubleLinkedList(face.vertices:_*)
    val edgesToAdd = new ListBuffer[(V, V)]()

      dll.moveTo(uJ)
      val uJM1 = dll.previous

      dll.moveTo(u1)

      var current = u1
      while (current != uJM1) {
        if (current != w || (current == w && (commonNeighbors(current, uJ, edgesToAdd, embedding) diff Vector(dll.getPrevious, dll.getNext)).isEmpty)) {
          edgesToAdd += ((uJ, current))
        }
        current = dll.next
      }


    dll.moveTo(uP)
    val uPM1 = dll.previous
    if (uJ == uPM1 || !embedding.embeddingFor(uJ).containsEntryFor(uP)) {
      edgesToAdd += ((v, uJ))

    } else {
      val uKCandidate = maximalKNotAdjacentToUJ(uJ, uP, dll, embedding)
      val uK = if (uKCandidate == uJ) {
        dll.moveTo(uJ)
        dll.getNext
      } else uKCandidate

      // Add the edge u(k) -> u1
      if (uK != v)
        edgesToAdd += ((u1, uK))

      // Add edges from v to u(k) ... u(p - 1)
      dll.moveTo(uK)
      var current = dll.value // Starts at u(k)
      do {
        if (v != current && !embedding.embeddingFor(v).containsEntryFor(current))
          edgesToAdd += ((v, current))

        current = dll.next
      } while (current != uP)
    }

    Augmentation(edgesToAdd.toVector)
  }

  private def maximalKNotAdjacentToUJ[V](uJ: V, uP: V, face: DoubleLinkedList[V], embedding: Embedding[V]): V = {
    val start = face.value
    face.moveTo(uP)
    var i = 0
    while (i < face.size) {
      val moveBack: V = face.previous

      if (moveBack == uJ || (moveBack != uJ && !embedding.embeddingFor(moveBack).containsEntryFor(uJ))) {
        face.moveTo(start)
        return moveBack
      }

      i += 1
    }

    throw new Error("No maximal k such that u(j) is not a neighbor of k found.")
  }

  /* Quick and dirty use of a graph structure to ad the relevant edges from the embedding to check neighbors. */
  private def commonNeighbors[V : TypeTag](a: V, b: V, addedEdges: ListBuffer[(V, V)], emb: Embedding[V]): Vector[V] = {
    val newEdges = addedEdges.toVector.map(t => t._1~t._2)
    val embeddingEdges = emb.embeddingFor(a).toVector.map(_.adjacentVertex~a) ++ emb.embeddingFor(b).toVector.map(_.adjacentVertex~b)
    val g = Graph.from(Nil, embeddingEdges ++ newEdges)
    val newNeighbors = if (g.contains(a) && g.contains(b)) {
      GraphCommons.outerNeighbors(a, g) intersect GraphCommons.outerNeighbors(b, g)
    } else {
      Vector()
    }
    newNeighbors
  }
}
