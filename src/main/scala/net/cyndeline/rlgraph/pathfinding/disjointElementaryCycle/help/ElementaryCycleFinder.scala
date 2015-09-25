package net.cyndeline.rlgraph.pathfinding.disjointElementaryCycle.help

import net.cyndeline.rlgraph.cycles.{Cycle, SingleCycleFinder, UndirectedSingleCycleFinder}
import net.cyndeline.rlgraph.triangulation.minimumDegree.faceTriangulation.multiEdgePrevention.help.VisitedVertex
import net.cyndeline.rlgraph.util.{DeadEndTrimmer, GraphConverter, GraphConverterI}

import scala.collection.mutable
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Uses a modified version of Hierholzers algorithm to find an elementary cycle in a graph that isn't guaranteed
 * to allow an Eulerian cycle. The modification accounts for the following to cases:
 *
 *  A. A given vertex may not have an initial closed tour starting and ending in that vertex
 *  A. A vertex v on the current cycle may have outgoing unvisited edges, but no tour back to v
 *
 * Algorithm description:
 *
 *  1. Remove all dead ends, making the graph biconnected or empty.
 *   1. If the graph is empty, no elementary cycle exists.
 *  1. Compute an arbitrary main cycle from the graph where each vertex only appears once.
 *
 * If an initial cycle is found using the above algorithm:
 *
 *  1. Set the main cycle as the current cycle C.
 *  1. Mark every non-visited vertex in C as visited.
 *  1. For each vertex V in C that has 2 or more unvisited neighbors, check if a cycle exists that doesn't involve
 *     a visited vertex other than V.
 *   1. If a cycle C2 exists, merge it with C at the vertex V.
 *   1. Set C2 as C and repeat until no vertices has unvisited neighbors.
 *
 * @param cycleFinder Parses cycles with no vertex repetitions.
 * @param graphConverter Converts input graphs into graphs that allows vertices to be marked as visited.
 * @tparam VType Vertex type in the input graph.
 * @tparam EType Edge type in the input graph
 */
class ElementaryCycleFinder[VType : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VisitedVertex[VType]]]})#l]
  (cycleFinder: SingleCycleFinder[VisitedVertex[VType], UnDiEdge],
   graphConverter: GraphConverterI[VType, VisitedVertex[VType], EType, UnDiEdge]) {

  /**
   * Constructs an elementary cycle finder using default sub-cycle parsers and graph pre-processing classes.
   * @return A default ElementaryCycleFinder.
   */
  def this() = this(new UndirectedSingleCycleFinder[VisitedVertex[VType], UnDiEdge]((v: VisitedVertex[VType]) => !v.isVisited),
                    GraphConverter[VType, VisitedVertex[VType], EType, UnDiEdge]((v: VType) => VisitedVertex(v), (e: EType[VType], v1: VisitedVertex[VType], v2: VisitedVertex[VType]) => v1~v2))

  /**
   * Finds an elementary cycle from a graph that may not allow Eulerian cycles.
   * @param graph An undirected graph.
   * @return An elementary cycle that consists of some cycle plus all sub-cycles that can be joined to it, and
   *         any cycles that results from each join.
   */
  def findCycle(graph: Graph[VType, EType]): Option[Vector[VType]] = {
    val visitationGraph: Graph[VisitedVertex[VType], UnDiEdge] = graphConverter.convert(graph)
    val graphWithoutDeadEnds = DeadEndTrimmer.trim(visitationGraph) // These won't be part of a cycle anyway

    // No cycles = empty graph
    if (graphWithoutDeadEnds.isEmpty)
      return None

    // This is the cycle that will be updated during the algorithm
    val arbitraryStart: VisitedVertex[VType] = graphWithoutDeadEnds.nodes.head
    var mainCycle: Vector[VisitedVertex[VType]] = cycleFinder.findCycleFromVertex(arbitraryStart, graphWithoutDeadEnds).get.vertices // At least one cycle, or graph would be empty
    val verticesToProcess = new mutable.Queue[VisitedVertex[VType]]()
    processCycleVertices(mainCycle, graphWithoutDeadEnds, verticesToProcess)

    while (!verticesToProcess.isEmpty) {
      val potentialSubListStart = verticesToProcess.dequeue()
      val subCycle: Option[Cycle[VisitedVertex[VType]]] = cycleFinder.findCycleFromVertex(potentialSubListStart, graphWithoutDeadEnds)
      if (subCycle.isDefined) {

        /* Since the list start already exists in the main cycle, the new list can't have it in the beginning.
         * On the other hand, since the start vertex now is visited an additional time, it needs to be at the
         * end of the new cycle as well.
         */
        val newCycle = subCycle.get.vertices.drop(1) :+ potentialSubListStart
        mainCycle = insertListIntoMainCycle(newCycle.toList, potentialSubListStart, mainCycle.toList).toVector
        processCycleVertices(newCycle, graphWithoutDeadEnds, verticesToProcess)
      }

    }

    Some(mainCycle.map(_.vertex))
  }

  /* In order for a vertex that is a member of one or more cycles to allow additional cycles, it must have at least
   * two neighbors that haven't been visited yet (one in and one out when traversing the new cycle).
   */
  private def cycleVertexCanHaveSubCycle(vertex: VisitedVertex[VType], graph: Graph[VisitedVertex[VType], UnDiEdge]): Boolean = {
    if (!vertex.isVisited)
      throw new Error(vertex + " was not visited. Sub cycles can only be computed from visited vertices already on a cycle.")

    graph.get(vertex).neighbors.count(!_.isVisited) >= 2
  }

  /* Marks every vertex in a cycle as visited, and adds it to a queue if it can hold additional cycles of unvisited vertices. */
  private def processCycleVertices(cycle: Vector[VisitedVertex[VType]],
                                          graph: Graph[VisitedVertex[VType], UnDiEdge],
                                          queue: mutable.Queue[VisitedVertex[VType]]) {
    for (vertex <- cycle) {
      vertex.markAsVisited()

      if (cycleVertexCanHaveSubCycle(vertex, graph)) {
        queue.enqueue(vertex)
      }
    }
  }

  private def insertListIntoMainCycle[E](listToInsert: List[E], elementToInsertAt: E, insertInto: List[E]): List[E] = {
    insertInto match {
      case (x : E)::(xs : List[E]) => if (x == elementToInsertAt) {
          x +: (listToInsert ++ xs)
        } else {
          x +: insertListIntoMainCycle(listToInsert, elementToInsertAt, xs)
        }
      case Nil => throw new Error("The element " + elementToInsertAt + " was not found in the insert list.")
    }
  }

}
