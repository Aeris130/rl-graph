package net.cyndeline.rlgraph.triconnectivity.subdivision2D

import net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.BiconnectivityOperation
import net.cyndeline.rlgraph.connectivity.PlanarConnect
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.triconnectivity.GraphTriconnection

import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

/**
  * Triconnects a planar graph by adding a vertex v inside every face, and connecting the v to every vertex on the
  * boundary of its face.
  */
class SubdivisionTriconnector {

  /**
    * @param g A graph g to triconnect.
    * @param vertexFactory Called once for every additional vertex that needs to be produced.
    * @return The triconnected graph, and the edges that were added.
    */
  def triconnectGraph(g: Graph[Int, UnDiEdge], vertexFactory: () => Int): GraphTriconnection[Int] = {
    runAlgorithm(g, vertexFactory, None)
  }

  /**
    * Lets the user specify an outer face to avoid adding edges to.
    * @param outer Returns the desired outer face from a list of faces.
    */
  def triconnectGraphWithOuterFace(g: Graph[Int, UnDiEdge], vertexFactory: () => Int, outer: Vector[Face[Int]] => Face[Int]): GraphTriconnection[Int] = {
    runAlgorithm(g, vertexFactory, Some(outer))
  }

  private def runAlgorithm(g: Graph[Int, UnDiEdge], vertexFactory: () => Int, outer: Option[Vector[Face[Int]] => Face[Int]]): GraphTriconnection[Int] = {
    if (g.isEmpty)
      return GraphTriconnection(g, Vector(), Vector())

    val extraEdges = new ArrayBuffer[(Int, Int)]()
    val extraVertices = new ArrayBuffer[Int]()

    val connectedGraph = if (g.isConnected) g else {
      val bicon = new BiconnectivityOperation[Int]().biconnect(g)
      extraEdges ++= bicon.extraEdges
      bicon.graph
    }

    val faces = new FaceComputation[Int]().computeFacesFromGraph(connectedGraph)
    val outerFace = if (outer.isDefined) Some(outer.get(faces)) else None
    var triconnectedGraph = connectedGraph


    val fs = faces.iterator
    while (fs.hasNext) {
      val face = fs.next()
      if (face.vertexSize > 3 && (outerFace.isEmpty || outerFace.get != face)) {
        val faceVertex = vertexFactory()
        extraVertices += faceVertex
        val newEdges = face.vertices.map(v => v~faceVertex)
        extraEdges ++= newEdges.map(e => (e._1, e._2))
        triconnectedGraph ++= newEdges // Vertices gets added here too
      }
    }

    GraphTriconnection(triconnectedGraph, extraVertices.toVector, extraEdges.toVector)
  }

}
