package net.cyndeline.rlgraph.drawings.planar.rectangular.embedding.help

import scala.reflect.ClassTag
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge

/**
 * Checks that a biconnected component of the graph can be embedded correctly. This is only the case if the component
 * has 1 or 2 common vertices for every edge, and the number of interior triangles are equal to the number of
 * interior faces.
 *
 * This validation is used to speed up the algorithm by identifying non-valid graphs early on and exiting before
 * the embedding takes place.
 *
 * Common vertex: A vertex neighboring both ends of an edge in the graph
 *
 * #Interior faces = |E(G)| - |V(G)| + 1
 * #Interior triangles = (total number of common vertices) / 3
 */
//TODO test
class ComponentValidator {
  private val vertexFinder = new CommonVertexFinder()

  /**
   * Checks if a biconnected component of a graph permits the Bhasker Sahni embedding.
   * @param graph Biconnected component.
   * @tparam VType Vertex type in the graph.
   * @return The number of common vertices in the component if a valid embedding exist, otherwise None. This number is
   *         needed after the embedding has finished to ensure that the correct number of triangles were embedded.
   */
  def hasEmbedding[VType : ClassTag, EType[X] <: UnDiEdge[X]](graph: Graph[VType, EType]): Option[Int] = {
    val edges = graph.edges.iterator
    var totalCommonVertices = 0
    while (edges.hasNext) {
      val edge = edges.next()
      val commons: Int = vertexFinder.findCommonVertices(edge.toOuter, graph).size

      if (commons < 1 || commons > 2)
        return None
      else
        totalCommonVertices += commons
    }

    if (graph.edges.size - graph.nodes.size + 1 == totalCommonVertices / 3)
      Some(totalCommonVertices)
    else
      None
  }
}
