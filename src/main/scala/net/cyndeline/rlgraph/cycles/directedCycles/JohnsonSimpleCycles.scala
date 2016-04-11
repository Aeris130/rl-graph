package net.cyndeline.rlgraph.cycles.directedCycles

import net.cyndeline.rlgraph.cycles.{Cycle, DirectedSimpleCycles}
import net.cyndeline.rlgraph.subgraph.stronglyConnectedComponents.TarjansAlgorithm
import net.cyndeline.rlgraph.util.{GraphCommons, IndexGraph, IndexVertex}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

/**
 * Computes every simple cycle in a directed graph using the algorithm by Donald B. Johnson. Runtime complexity
  * O(((V+E)C).
 *
 * @param maxSize The highest amount of vertices permitted in a single cycle. Larger cycles will be discarded.
 */
class JohnsonSimpleCycles[V : TypeTag] private (maxSize: Option[Int]) extends DirectedSimpleCycles[V] {
  private def tarjansAlg = new TarjansAlgorithm()

  /**
   * Constructs a default cycle algorithm with no maximum cycle length.
   */
  def this() = this(None)

  /**
   * Constructs a cycle algorithm that discards cycles above the maximum cycle size.
   * @param maxSize The highest amount of vertices permitted in a single cycle. Larger cycles will be discarded.
   */
  def this(maxSize: Int) = this(Some(maxSize))

  def findCycles(graph: Graph[V, DiEdge]): Vector[Cycle[V]] = {
    val cycles = new ListBuffer[Cycle[V]]()
    def ff(c: Cycle[V]): Unit = {
      cycles += c
    }
    iterateCycles(graph, ff)
    cycles.toVector
  }

  def iterateCycles(graph: Graph[V, DiEdge], f: Cycle[V] => Unit): Unit = {
    if (graph.isEmpty)
      return

    val indexGraph = IndexGraph.directed(graph)
    val outerVertices = GraphCommons.outerVertices(indexGraph)
    val data = new Data(outerVertices)
    val highestIndex = outerVertices.maxBy(_.index).index
    val indexMap = outerVertices.map(v => v.index -> v).toMap
    var lowestVertex = outerVertices.minBy(_.index)
    val stop = highestIndex

    while (lowestVertex.index < stop) {
      val subGraph = indexGraph filter indexGraph.having(node = _.index >= lowestVertex.index)
      val strongConnectedComponent = leastSCC(subGraph)

      if (strongConnectedComponent.isDefined) {
        val component = strongConnectedComponent.get
        lowestVertex = component.nodes.minBy(_.index)
        for (n <- component.nodes) {
          data.unblock(n)
          data.clearBlockedNeighbors(n)
        }

        circuit(data, lowestVertex, lowestVertex, component, f)
        lowestVertex = indexMap(lowestVertex.index + 1)
      } else {
        lowestVertex = indexMap(stop)
      }

    }
  }

  private def circuit(data: Data,
                      v: IndexVertex[V],
                      s: IndexVertex[V],
                      graph: Graph[IndexVertex[V], DiEdge],
                      register: Cycle[V] => Unit): Boolean = {
    if (graph.isEmpty)
      return false

    var f = false
    data.push(v)
    data.block(v)

    for (n <- GraphCommons.outgoingOuterNeighbors(v, graph)) {
      if (n == s) {
        if (stackHasValidSize(data.stack)) {
          register(Cycle(data.stack.toVector.map(_.valueOfIndex).reverse))
        }
        f = true

      } else if (!data.isBlocked(n)) {

        if (circuit(data, n, s, graph, register)) {
          f = true
        }

      }
    }

    if (f) {
      data.unblock(v)
    } else {
      for (n <- GraphCommons.outgoingOuterNeighbors(v, graph) if !data.hasBlockedNeighbor(n, v)) {
        data.addBlockedNeighbor(n, v)
      }
    }

    data.pop()
    f
  }

  /**
   * Computes the sub-graph S of a graph G, where S contains the vertices and edges that make up the strongly connected
   * component of G that contains the vertex in G with the lowest index.
   * @param graph A non-empty graph.
   * @return A strongly connected component of the input graph, containing the vertex having the lowest index. None if
   *         only components with a single vertex are found (no cycles can exist in those).
   */
  private def leastSCC(graph: Graph[IndexVertex[V], DiEdge]): Option[Graph[IndexVertex[V], DiEdge]] = {
    val stronglyConnectedComponents = tarjansAlg.sccComponents(graph)
    val cycleComponents = stronglyConnectedComponents
      .filter(_.size > 1)

    if (cycleComponents.isEmpty) {
      None
    } else {
      val lowestComponent = cycleComponents
        .map(component => (component, component.minBy(_.index).index))
        .minBy(_._2)
        ._1.toSet

      Some(graph filter graph.having(node = lowestComponent.contains(_)))
    }
  }

  private def stackHasValidSize(stack: mutable.Stack[IndexVertex[V]]): Boolean = {
    maxSize.isEmpty || (stack.size <= maxSize.get)
  }

  private class Data(vertices: Vector[IndexVertex[V]]) {
    var blocked: Vector[Boolean] = Vector.fill(vertices.size)(false)
    var blockedNeighbors: Vector[Set[IndexVertex[V]]] = Vector.fill(vertices.size)(Set[IndexVertex[V]]())
    val stack = new mutable.Stack[IndexVertex[V]]()

    def push(v: IndexVertex[V]) {
      stack.push(v)
    }

    def pop(): IndexVertex[V] = {
      stack.pop()
    }

    def block(v: IndexVertex[V]) {
      blocked = blocked.updated(v.index, true)
    }

    def unblock(v: IndexVertex[V]) {
      blocked = blocked.updated(v.index, false)
      val blockedNs = blockedNeighbors(v.index).iterator

      while (blockedNs.hasNext) {
        val neighbor = blockedNs.next()
        if (isBlocked(neighbor))
          unblock(neighbor)
      }

      clearBlockedNeighbors(v)
    }

    def isBlocked(v: IndexVertex[V]): Boolean = {
      blocked(v.index)
    }

    def hasBlockedNeighbor(v: IndexVertex[V], n: IndexVertex[V]): Boolean = {
      blockedNeighbors(v.index).contains(n)
    }

    def addBlockedNeighbor(v: IndexVertex[V], n: IndexVertex[V]) {
      blockedNeighbors = blockedNeighbors.updated(v.index, blockedNeighbors(v.index) + n)
    }

    def clearBlockedNeighbors(v: IndexVertex[V]) {
      blockedNeighbors = blockedNeighbors.updated(v.index, Set())
    }
  }

}
