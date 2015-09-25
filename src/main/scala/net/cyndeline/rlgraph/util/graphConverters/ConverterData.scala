package net.cyndeline.rlgraph.util.graphConverters

import scalax.collection.GraphPredef._

/**
 * Contains the output from converting a scala graph to another graph structure. Also serves as input when
 * converting another graph structure to scala graph.
 * @param convertedGraph Graph structure other than scala graph. If the data is produced when converting from
 *                       scala-graph to another structure, this is the graph resulting from the conversion.
 *                       When sending this data as input, this is the graph that should be converted to scala-graph.
 * @param vertexMap Maps every vertex in the converted graph to the vertex that should be used in scala-graph.
 * @param edgeMap Maps every edge in the converted graph to the edge that should be used in scala-graph.
 * @tparam VType Vertex type used in scala-graph.
 * @tparam EType Edge type used in scala-graph.
 * @tparam OtherGraph Type of graph converted to from scala-graph.
 * @tparam OtherVType Vertex type used in OtherGraph.
 * @tparam OtherEType Edge type used in OtherGraph.
 */
case class ConverterData[VType, EType[X] <: EdgeLikeIn[X], OtherGraph, OtherVType, OtherEType]
  (convertedGraph: OtherGraph,
   vertexMap: Map[VType, OtherVType],
   edgeMap: Map[EType[VType], OtherEType]) {

}
