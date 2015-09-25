package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network

import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdge, FlowEdgeAssoc, FlowVertex, NetworkRepresentation}

import scala.collection.mutable
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Constructs a flow network using a planar embedding, where every vertex in the embedded graph
 * will be present in the network in the set of vertices Nv. An additional vertex will be added to
 * the network for each face in the embedding to the set Nf. (Nv U Nf) will make up the total
 * set of network vertices, and every vertex in Nv will have a directed edge to every face Nf it is a part of.
 * Every vertex in Nf will also have an additional edge to every adjacent face vertex.
 *
 * Every v <- Nv will have production 4
 * Every v <- Nf will have consumption 2 * a(f) + x
 *
 *  a(f) = the face that v belongs to
 *  x = -4 if f is internal, +4 if it's external
 *
 *  @constructor Creates a new flow network constructor.
 */
class FlowNetworkConstructor {

  def construct[VType](vertices: Vector[VType], faces: Vector[Face[VType]], externalFace: Int): NetworkRepresentation[VType] = {
    implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
    val networkRep = new NetworkRepresentation[VType]()
    var network = Graph[FlowVertex, FlowEdge]()
    var vertexId = 0
    var edgeId = 0

    /*
     * Set up a network vertex for every face and vertex belonging to the original graph.
     */

    for (vertex <- vertices) {
      val fv = new FlowVertex(vertexId, 4)
      vertexId += 1
      networkRep.vertexMapping put (fv, vertex)
      network += fv
    }

    var n = 0
    while (n < faces.size) {
      val face = faces(n)
      val consumption = if (n != externalFace) (face.vertexSize * 2) - 4 else (face.vertexSize * 2) + 4
      val fv = new FlowVertex(vertexId, -consumption)
      vertexId += 1
      networkRep.faceMapping put (fv, face)
      network += fv

      if (n == externalFace) networkRep.externalFace = fv

      n += 1
    }

    /*
     * Add an edge from every vertex to every face that vertex belongs to.
     */
    val faceToVertex: mutable.HashMap[Face[VType], FlowVertex] = networkRep.faceMapping.map(_.swap)  // Key and values reversed
    val vTypeToFlowVertex: mutable.HashMap[VType, FlowVertex] = networkRep.vertexMapping.map(_.swap)
    for (face <- faces) {
      val faceVertex = faceToVertex.get(face).get
      var visitedVertices = Set[VType]()

      for (v <- face.vertices) {
        if (!visitedVertices.contains(v)) {
          val vertex = vTypeToFlowVertex.get(v).get
          val lowerBound = face.vertices.count(_ == v) // lb equal to the number of angles around the vertex, inside the face
          val e: FlowEdge[FlowVertex] = vertex ~> faceVertex ## (edgeId, lowerBound, 4, 0, 0, None) // Lower bound / capacity / cost
          edgeId += 1
          visitedVertices += v

          network += e
        }
      }
    }

    /*
     * Add an edge from every face to every other adjacent face. This involves routing edges to itself
     * if the face contains a bridge.
     */

    var edgeMapping = Map[(VType, VType), FlowVertex]() // Which face vertex each edge a -> b belongs to. Different from b -> a.
    n = 0

    /* Map every directed edge vertex pair to the face it belongs to in the network. */
    while (n < faces.size) {
      var face = faces(n).vertices
      edgeMapping = edgeMapping + ((face.last, face.head) -> faceToVertex.get(faces(n)).get) // Since lists don't wrap around

      while (face.size >= 2) {
        edgeMapping = edgeMapping + ((face(0), face(1)) -> faceToVertex.get(faces(n)).get)
        face = face.drop(1)
      }

      n += 1
    }

    /* Find every vertex-pair belonging to a face, draw an edge to the face-vertex that the inverse of the pair belongs to. */
    n = 0
    while (n < faces.size) {
      var face = faces(n).vertices
      var edge: (VType, VType) = (face.last, face.head)

      do {
        // Face of edge ~> face of opposite edge
        network += edgeMapping.get(edge).get ~> edgeMapping.get(edge.swap).get ## (edgeId, 0, -1, 1, 0, Option(edge)) // id, lower bound / infinite capacity / cost / flow
        edgeId += 1

        if (face.size >= 2) {
          edge = (face(0), face(1))
          face = face.drop(1)
        } else {
          edge = null
        }
      } while (edge != null)

      n += 1
    }

    networkRep.graph = network
    networkRep
  }
}
