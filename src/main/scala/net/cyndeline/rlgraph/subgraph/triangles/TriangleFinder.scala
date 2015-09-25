package net.cyndeline.rlgraph.subgraph.triangles

import net.cyndeline.rlgraph.util.{GraphCommons, GraphConverter}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Parses triangles from a graph such that no two triangles contains the same set of vertices.
 */
class TriangleFinder {

  /**
   * @param graph Graph to parse triangles from.
   * @return A list of triples representing each found triangle.
   */
  def computeTriangles[VType : TypeTag : ClassTag](graph: Graph[VType, UnDiEdge]): Vector[Triangle[VType]] = {
    val markedGraph = GraphConverter.toUndirectedWithNodes[VType, Mark[VType], UnDiEdge]((v: VType) => Mark[VType](v)).convert(graph)
    val nodeList = markedGraph.nodes.toVector.map(n => {
      val outer: Mark[VType] = n
      (outer, n.degree)
    }).toVector
    val decreasingDegree = nodeList.sortWith(_._2 > _._2) // Highest degree first
    val triangles = new ListBuffer[Triangle[VType]]()

    var current = markedGraph
    for (mark <- decreasingDegree) {
      val stableRef = current
      val neighbors = GraphCommons.neighbors[Mark[VType], UnDiEdge](mark._1, stableRef)
      neighbors.foreach(_.mark())

      for (n1 <- neighbors) {
        val stableRef = current
        val n1Neighbors = GraphCommons.neighbors[Mark[VType], UnDiEdge](n1, stableRef)
        for (n2 <- n1Neighbors) {
          if (n2.isMarked) {
            triangles += Triangle(mark._1.markValue, n1.markValue, n2.markValue)
          }
        }
        n1.unMark()
      }
      current -= mark._1
    }

    triangles.toVector
  }

}
