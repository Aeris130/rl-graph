package net.cyndeline.rlgraph.drawings.planar.orthogonal.representation

import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util.{FlowEdge, FlowVertex, NetworkRepresentation}
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.darts.DefaultDart
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.{Dart, OFace, VertexWrapper, VertexWrapperFactory}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scalax.collection.immutable.Graph

/**
 * Creates faces of an orthogonal graph, consisting of a circular list of darts, based on
 * the flow of a network.
 *
 * @constructor Creates a new orthogonal face factory.
 * @tparam VType Type of vertex used in the original graph.
 */
class OrthogonalFaceFactory[VType](wrapperFactory: VertexWrapperFactory[VType]) {

  /**
   * Creates a set of faces using shared wrappers for vertices using the same value.
 *
   * @param faces A set of faces containing the order its vertices are traversed around it.
   * @param externalFace Specifies which face member is the external face in its graph.
   * @param network Flow network representation of the graph each face belongs to.
   * @return a set of faces with orthogonal darts in the order they're traversed around the face, as well as the
   *         external orthogonal face.
   */
  def createFaces(faces: Set[Face[VType]],
                         externalFace: Face[VType],
                         network: NetworkRepresentation[VType]): (Set[OFace[VType]], OFace[VType]) = {
    if (!faces.contains(externalFace))
      throw new IllegalArgumentException("The external face " + externalFace + " was not present in the face set " + faces)

    var vertexWrappers = Map[VType, VertexWrapper[VType]]()
    var oFaces = Set[OFace[VType]]()
    var externalOFace: OFace[VType] = null
    val fs = faces.iterator

    while (fs.hasNext) {
      val face = fs.next()
      val oFace = createFace(face, network, vertexWrappers)

      oFace.vertices.foreach(v => {
        if (!vertexWrappers.contains(v.value.get)) {
          vertexWrappers += (v.value.get -> v)
        }
      })

      oFaces += oFace

      if (face == externalFace)
        externalOFace = oFace
    }

    (oFaces, externalOFace)
  }

  /**
   * Creates a single face of orthogonal darts based on the face of a planar graph.
 *
   * @param face face containing the order its vertices are traversed around it.
   * @param network Flow network representation of the graph the face belongs to.
   * @param previousWrappers If a vertex has already been created in another face, its wrapper
   *                         is mapped here so it can be reused.
   * @return a face with orthogonal darts in the order they're traversed around the face.
   */
  def createFace(face: Face[VType],
                        network: NetworkRepresentation[VType],
                        previousWrappers: Map[VType, VertexWrapper[VType]] = Map[VType, VertexWrapper[VType]]()): OFace[VType] = {
    val vertexToFlowVertex: mutable.HashMap[VType, FlowVertex] = network.vertexMapping.map(_.swap)

    /* Add the previous wrappers to the mapping. */
    var vertexToWrapper: Map[VType, VertexWrapper[VType]] = {
      var m = Map[VType, VertexWrapper[VType]]()
      for (keyValue <- previousWrappers)
        m += (keyValue._1 -> keyValue._2)

      m
    }
    val faceNode: FlowVertex = network.faceMapping.map(_.swap).get(face).get
    val darts = new ArrayBuffer[Dart[VType]]

    /* Stores how many edges that are left that shares vertex angles around a vertex, and how much flow
     * there is left to share.
     */
    var sharedFlow = Map[VType, (Set[(VType, VType)], Int)]()

    /* If a cut point exists in a face, one or more vertices will be traversed more than once.
     * Since each vertex only has a single flow value specified, it is up to this algorithm to
     * split the flow between every vertex angle around any single vertex that needs it.
     *
     * Find every vertex with n edges going into it (if n > 1), then assign Floor(total_flow / n)
     * whenever a dart needs it, until only one dart remains (it gets the rest).
     */
    val incomingEdges = new ArrayBuffer[(VType, VType)]()
    for (vertex <- face.vertices) {
      incomingEdges.clear()

      for (edge <- face.edges) {
        if (edge._2 == vertex) incomingEdges += edge
      }

      if (incomingEdges.size > 1) {
        val flowToShare = findEdge(vertexToFlowVertex(vertex), faceNode, network.graph).flow
        sharedFlow += (vertex -> (incomingEdges.toSet, flowToShare))
      }
    }

    for (edge <- face.edges) {
      val to: FlowVertex = vertexToFlowVertex(edge._2)
      val bendAngle = findCrossingEdge(edge._1, edge._2, network.graph).flow

      /* If this is the only dart going into this vertex, give the bend all flow.
       * Otherwise, allot the dart a share of the total flow.
       */
      if (!sharedFlow.contains(edge._2)) {
        val vertexAngle = findEdge(to, faceNode, network.graph).flow
        val dartAndWrappers = makeDart(edge._1, edge._2, vertexAngle, bendAngle, vertexToWrapper)
        darts += dartAndWrappers._1
        vertexToWrapper = dartAndWrappers._2

      } else {
        val edgesAndFlow: (Set[(VType, VType)], Int) = sharedFlow(edge._2)
        val allIncomingEdges = edgesAndFlow._1
        val flowToAllocate = Math.floor(edgesAndFlow._2 / allIncomingEdges.size).toInt

        if (allIncomingEdges.size < 2) {
          sharedFlow -= edge._2
        } else {
          sharedFlow += (edge._2 -> (allIncomingEdges - edge, edgesAndFlow._2 - flowToAllocate))
        }

        val dartAndWrappers = makeDart(edge._1, edge._2, flowToAllocate, bendAngle, vertexToWrapper)
        darts += dartAndWrappers._1
        vertexToWrapper = dartAndWrappers._2
      }

    }

    new OFace(darts.toVector)
  }

  /**
   * Creating darts has to be done using the same vertex wrappers as each vertex has had created before.
   */
  private def makeDart(from: VType, to: VType, flow: Int, bends: Int, wrappers: Map[VType, VertexWrapper[VType]]): (Dart[VType], Map[VType, VertexWrapper[VType]]) = {
    var newWrappers = wrappers

    val fromWrapper = wrappers.getOrElse(from, {
      val w = wrapperFactory.createVertexWrapper(from)
      newWrappers += (from -> w)
      w
    })
    val toWrapper = wrappers.getOrElse(to, {
      val w = wrapperFactory.createVertexWrapper(to)
      newWrappers += (to -> w)
      w
    })
    (new DefaultDart(fromWrapper, toWrapper, flow, bends), newWrappers)
  }

  /**
   * Finds an edge in a graph.
   */
  private def findEdge(from: FlowVertex, to: FlowVertex, graph: Graph[FlowVertex, FlowEdge]): FlowEdge[FlowVertex] = {
    val innerTo = graph.get(to)
    for (outgoing <- graph.get(from).outgoing) {
      if (outgoing.to == innerTo) return outgoing.toOuter
    }

    throw new Error("No edge found going from vertex " + from + " to " + to + " in graph " + graph)
  }

  /**
   * Finds a flow edge that crosses an edge between two vertices.
   */
  private def findCrossingEdge(from: VType, to: VType, graph: Graph[FlowVertex, FlowEdge]): FlowEdge[FlowVertex] = {
    val allEdges = graph.edges.iterator
    while (allEdges.hasNext) {
      val edge = allEdges.next().toOuter
      if (edge.crosses.isDefined && edge.crosses.get == (from, to)) return edge
    }

    throw new Error("No edge found crossing an edge between " + from + " and " + to + " in graph " + graph)
  }

}
