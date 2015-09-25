package net.cyndeline.rlgraph.cycles

import net.cyndeline.rlgraph.util.graphConverters.jGraphT.ScalaGraphToJGraphTConverter
import org.jgrapht.alg.cycle.SzwarcfiterLauerSimpleCycles
import org.jgrapht.graph.DefaultEdge

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

/**
 * Wrapper for JGraphT's directed cycle finder that runs in O(V+E) time for each cycle.
 */
class DirectedCycleFinder[V, E[X] <: DiEdge[X]](maxSize: Int) extends CycleFinder[V, E] {
  private val converter = new ScalaGraphToJGraphTConverter()

  def findCycles(graph: Graph[V, E]): Vector[Cycle[V]] = {
    val cycleFinder = new SzwarcfiterLauerSimpleCycles[V, DefaultEdge]()
    val diGraph = converter.convertToDirected(graph)
    cycleFinder.setGraph(diGraph)
    val cycles = cycleFinder.findSimpleCycles().iterator()
    val scalaCycles = new ListBuffer[Cycle[V]]()

    while (cycles.hasNext) {
      val c = cycles.next()
      if (c.size() <= maxSize)
        scalaCycles += Cycle(c.toVector)
    }

    scalaCycles.toVector
  }
}
