package net.cyndeline.rlgraph.drawings.planar.rectangular.help

import net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.BiconnectivityOperation
import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.planar.boyerMyrwold.BoyerMyrwoldEmbedder
import net.cyndeline.rlgraph.drawings.planar.rectangular.OuterFaceSelection
import net.cyndeline.rlgraph.drawings.planar.rectangular.dataStructure.RVertex
import net.cyndeline.rlgraph.drawings.planar.rectangular.embedding.BhaskerSahniEmbedder
import net.cyndeline.rlgraph.drawings.planar.rectangular.triangleBreak.TriangleBreaker
import net.cyndeline.rlgraph.triangulation.fourConnectivity.FourConnectivityTriangulation
import net.cyndeline.rlgraph.util.{GraphCommons, GraphConverter}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Performs the first four steps of the rectangular dual algorithm, by augmenting the input graph to ensure that it
 * can be used to produce the dual.
 */
class DataSetup[VType : ClassTag : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l]
  (graph: Graph[VType, EType], outerFaceSelect: Option[OuterFaceSelection[VType, EType]]) {

  private val data = setup

  val vNorth: RVertex[VType] = data._1
  val vSouth: RVertex[VType] = data._2
  val vWest: RVertex[VType] = data._3
  val vEast: RVertex[VType] = data._4
  val embedding: Embedding[RVertex[VType]] = data._5
  val splitPairs: Vector[(RVertex[VType], Vector[RVertex[VType]])] = data._6
  val dummyGates: Vector[RVertex[VType]] = data._7

  private def setup = {
    val embedder = new BhaskerSahniEmbedder[RVertex[VType]]() // Only embeds if the graph is a PTP graph
    val planarEmbed = new BoyerMyrwoldEmbedder[RVertex[VType]]()
    val biconnecter = new BiconnectivityOperation[RVertex[VType]]()
    val faceComp = new FaceComputation[RVertex[VType]]()

    /* Since the remaining algorithm requires additional vertices, the graph is converted to an internal
     * vertex format. The rest of the algorithm also only works with undirected edges.
     */
    val undirectedGraph = GraphConverter.toUndirected[VType, EType].convert(graph)
    val rvGraph: Graph[RVertex[VType], UnDiEdge] = convertToRVertexFormat(undirectedGraph)

    /* Step 1: Biconnect the graph */
    val biconnectedGraph = biconnecter.biconnect(rvGraph).graph
    var embedding = planarEmbed.embed(biconnectedGraph).getOrElse(throw new Error("Non-planarity induced via biconnection."))

    /* Step 2: Find an outer face and add the four vertices N,W,S,E to it. */
    val biconnectedFaces = faceComp.computeFaces(embedding)

    // The users face-selection requires non-RVertex faces
    val rToRegularFaceMap = biconnectedFaces.map(rf => new Face(rf.vertices.map(_.vertex)) -> rf).toMap
    val regularFaces = rToRegularFaceMap.keys.toVector
    val outerFace = if (outerFaceSelect.isDefined) outerFaceSelect.get.select(regularFaces, graph) else regularFaces.maxBy(_.vertexSize)
    val outerRFace: Face[RVertex[VType]] = rToRegularFaceMap(outerFace)

    val vSouth = RVertex.outer[VType]("South")
    val vNorth = RVertex.outer[VType]("North")
    val vWest = RVertex.outer[VType]("West")
    val vEast = RVertex.outer[VType]("East")

    val newOuterFaceAug = new OuterFaceAugmentation(outerRFace, vNorth, vEast, vSouth, vWest)
    embedding = newOuterFaceAug.augmentEmbedding(embedding)

    /* Step 3: Break all separating triangles */
    val breaker = new TriangleBreaker()
    embedding = breaker.breakTriangles(embedding)

    // Needed when reconstructing the final layout, since triangulation can add additional neighbors to a split
    val outerSet = Set(vNorth, vWest, vEast, vSouth)
    val splitPairs: Vector[(RVertex[VType], Vector[RVertex[VType]])] = for {
      split <- embedding.embeddedVertices.filter(_.isSplit)
      neighbors = embedding.embeddingFor(split).toVector.map(_.adjacentVertex)

      // In case an edge between an inner and one of the four outer vertices is selected for splitting
      if (neighbors.toSet intersect outerSet).isEmpty
    } yield split -> neighbors

    val dummyGates: Vector[RVertex[VType]] = for {
      split <- embedding.embeddedVertices.filter(_.isSplit)
      neighbors = embedding.embeddingFor(split).toVector.map(_.adjacentVertex)
      if !(neighbors.toSet intersect outerSet).isEmpty
    } yield split

    /* Step 4: Triangulate every face except the outer. */
    val triangulation = new FourConnectivityTriangulation[RVertex[VType]]()
    val augmentedOuterFace = Face(vNorth, vWest, vSouth, vEast)
    embedding = triangulation.triangulateWithoutFaces(embedding, Set(augmentedOuterFace))

    /* Step 4.1: Make sure that the graph is now valid before proceeding to the final step. */
    val rGraph = GraphCommons.embeddingAsGraph(embedding)
    embedder.embed(rGraph).getOrElse(
      throw new Error("Could not produce a valid embedding from the final PTP graph, despite selecting a valid outer face.")
    )

    (vNorth, vSouth, vWest, vEast, embedding, splitPairs, dummyGates)
  }

  private def convertToRVertexFormat(g: Graph[VType, UnDiEdge]): Graph[RVertex[VType], UnDiEdge] = {
    val allEdges = for (e <- g.edges) yield RVertex[VType](e._1)~RVertex[VType](e._2)
    Graph.from(Nil, allEdges)
  }

}
