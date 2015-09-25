package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.operation

import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.planar.boyerMyrwold.BoyerMyrwoldEmbedder
import net.cyndeline.rlgraph.planar.{ComputeFaceOperation, PlanarEmbedOperation}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.FlowNetworkConstructor
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.MaxFlowMinCost.MaxFlowMinCost
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.NetworkRepresentation
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.OrthogonalFaceFactory
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{OrthogonalDartRep, VertexWrapperFactory}
import net.cyndeline.rlgraph.util.GraphConverter

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Computes the base for an orthogonal drawing, the set of darts along each face, the angles one dart bends at
 * when transitioning to another dart on its face, and how many bends each dart has between its start and stop
 * vertices.
 *
 * @constructor Creates new representation computation.
 */
class OrthoRepresentationComputation[VType : TypeTag : ClassTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l]
  (wrapperFactory: VertexWrapperFactory[VType]){
  private val faceComputation: ComputeFaceOperation[VType] = new FaceComputation[VType]()
  private val planar: PlanarEmbedOperation[VType, UnDiEdge] = new BoyerMyrwoldEmbedder[VType]()
  private val flowNetworkComputation = new FlowNetworkConstructor()
  private val maxFlowMinCost = new MaxFlowMinCost()
  private val orthoFaceFactory = new OrthogonalFaceFactory(wrapperFactory)

  def computeRepresentation(graph: Graph[VType, EType]): OrthogonalDartRep[VType] = {

    /* No point in going any further if the graph isn't planar. */
    val embedding: Option[Embedding[VType]] = planar.embed(GraphConverter.toUndirected[VType, EType].convert(graph))
    if (embedding.isEmpty) throw new IllegalArgumentException("The graph " + graph + " was not planar.")

    /* The faces each make up a single sink in the network used to compute bends along edges. */
    val facesOfGraph: Vector[Face[VType]] = faceComputation.computeFaces(embedding.get)

    /* Likewise, each vertex is a sink with an edge going to every face it is a member of. */
    val allVertices: Set[VType] = (for { node <- graph.nodes } yield {
      val c: VType = node
      c
    })(collection.breakOut)

    /* Flow network used to compute edge-bends and angles at each vertex. Also contains mappings to the old
     * vertices and faces, to later check which vertex/face belongs to which flow-related data.
     */
    val externalFace = selectOuterFace(facesOfGraph)
    val flowNetwork: NetworkRepresentation[VType] = flowNetworkComputation.construct(allVertices.toVector, facesOfGraph, externalFace)

    /* Compute max-flow min-cost on the network. The flow going from a vertex in the original graph to a vertex
     * belonging to a face determines the bend that the graphs edges makes at that vertex, in that face.
     */
    val finalFlowNetwork = maxFlowMinCost.computeMaxFlowMinCost(flowNetwork.graph)
    flowNetwork.graph = finalFlowNetwork // Must be assigned separately due to identifier stability
    val orthoFaces = orthoFaceFactory.createFaces(facesOfGraph.toSet[Face[VType]], facesOfGraph(externalFace), flowNetwork)

    new OrthogonalDartRep[VType](orthoFaces._1, orthoFaces._2)
  }

  /**
   * Selects an outer face based on which face has the most vertices.
   *
   * @param faces All faces in a graph.
   * @return The position in the face list of the outer face.
   */
  private def selectOuterFace(faces: Vector[Face[VType]]): Int = {
    var mostEdges = 0
    var n = 0
    while (n < faces.size) {
      if (faces(n).vertexSize > faces(mostEdges).vertexSize) mostEdges = n

      n += 1
    }

    mostEdges
  }

}
