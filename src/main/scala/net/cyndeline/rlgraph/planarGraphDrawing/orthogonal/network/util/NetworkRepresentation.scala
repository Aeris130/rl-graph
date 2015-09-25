package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util

import net.cyndeline.rlgraph.face.Face

import scala.collection.mutable
import scalax.collection.immutable.Graph

/**
 * Stores the graph representation of a flow network, as well as additional
 * data used when generating it.
 *
 * @constructor Creates a new mutable network representation container.
 */
class NetworkRepresentation[VType] {

  var graph: Graph[FlowVertex, FlowEdge] = null
  var externalFace: FlowVertex = null

  val vertexMapping = new mutable.HashMap[FlowVertex, VType]()
  val faceMapping = new mutable.HashMap[FlowVertex, Face[VType]]()

  override def toString: String = {
    val builder = new StringBuilder()

    if (graph != null)
      builder ++= "Graph: " + graph.nodes.mkString(", ") + " ; " + graph.edges.mkString(", ")
    else
      builder ++= "No graph"

    builder ++= " | "

    if (externalFace != null)
      builder ++= "Ext face: " + externalFace
    else
    builder ++= "No external face"

    builder ++= " | "
    builder ++= vertexMapping.mkString(", ") + " | " + faceMapping.mkString(", ")
    builder.toString()
  }
}
