package net.cyndeline.rlgraph.util.graphConverters

import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.immutable.Graph

/**
 * Methods used when converting from/to scala graphs. When converting from another structure to scala-graph,
 * it is up to the implementation to supply a factory to produce new edges if an edge is found in the
 * other graph that lacks a mapping to scala-graph.
 *
 * @tparam VType Vertex type used in the scala graph.
 * @tparam EType Edge type used in the scala graph.
 * @tparam OtherGraph Type of graph to convert from/to.
 */
trait GraphConverter[VType, EType[X] <: EdgeLikeIn[X], OtherGraph, OtherVType, OtherEType] {

  /**
   * Converts from a scala graph to another graph structure.
   * @param graph A scala graph.
   * @return Graph structure type to convert to, as well as edges and vertices of the new graph mapped to the ones
   *         they're based off of in the input graph.
   */
  def convertTo(graph: Graph[VType, EType]): ConverterData[VType, EType, OtherGraph, OtherVType, OtherEType]

  /**
   * Converts another graph structure to scala graphs.
   * @param data Other graph type to convert to scala graph. Maps vertices in the input graph to the vertices used
   *             in scala-graph.
   * @return A scala graph structure based on the input graph.
   */
  def convertFrom(data: ConverterData[VType, EType, OtherGraph, OtherVType, OtherEType]): Graph[VType, EType]

}
