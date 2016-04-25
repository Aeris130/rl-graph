package net.cyndeline.rlgraph.planar.demoucron.operation

import net.cyndeline.rlgraph.biconnectivity.components.DFSComponentSearch
import net.cyndeline.rlgraph.cycles.UndirectedSingleCycleFinder
import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.pathfinding.Path
import net.cyndeline.rlgraph.planar.PlanarEmbedOperation
import net.cyndeline.rlgraph.planar.demoucron.operation.help.{Fragment, FragmentComputation, PathEmbedder, TreeEmbedMerger}
import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Embeds a connected undirected planar graph in O(n2) time using the algorithm by Demoucron, Malgrange, Pertuiset.
 *
 * @constructor Constructs a new demoucron embedding algorithm.
 */
class DemoucronEmbedding[VType: TypeTag : ClassTag, EType[X] <: UnDiEdge[X]] extends PlanarEmbedOperation[VType, EType] {
  private val faceComputation = new FaceComputation[VType]()
  private val fragmentComputation = new FragmentComputation[VType, UnDiEdge]()
  private val componentComputation = new DFSComponentSearch[VType, UnDiEdge]()
  private val merger = new TreeEmbedMerger[VType, UnDiEdge]()
  private val pathEmbedder = new PathEmbedder[VType]()
  private val cycleFinder = new UndirectedSingleCycleFinder[VType, UnDiEdge]()

  /**
   * Embeds a planar graph.
   * @param graph Graph to embed.
   * @return A planar embedding, or None if the graph isn't planar.
   */
  def embed(graph: Graph[VType, EType]): Option[Embedding[VType]] = {
    if (!graph.isConnected) throw new IllegalArgumentException("The supplied graph wasn't connected:\n" + graph)

    /* Euler's theorem */
    if (graph.nodes.size >= 3 && graph.edges.size > ((3 * graph.nodes.size) - 6)) return None // Not planar

    val undiGraph = makeUndirectedGraph(graph)
    val components = componentComputation.components(undiGraph)
    val compIt: Iterator[Graph[VType, UnDiEdge]] = components.iterator
    val embeddings = new ArrayBuffer[Embedding[VType]]()
    var embeddingsToSubGraphs = Map[Embedding[VType], Graph[VType, UnDiEdge]]()

    while (compIt.hasNext) {
      val component: Graph[VType, UnDiEdge] = compIt.next()

      /* Choose a cycle of G this is a planar graph G’ together with an embedding */
      val initialCycle = cycleFinder.findCycle(component)
      var embedding: Embedding[VType] = null

      if (initialCycle.isDefined) {
        val cycle: Vector[VType] = (for (n <- initialCycle.get.vertices) yield {
            val node: VType = n
            node
          }) :+ initialCycle.get.vertices.head

        var cycleGraph = undiGraph.empty
        for (edge <- initialCycle.get.edges) cycleGraph = cycleGraph + GraphCommons.outerEdgeNeighbors(edge._1, undiGraph).find(n => n._1 == edge._2).get._2

        embedding = pathEmbedder.embedCycle(cycle, UndirectedEmbedding[VType]())
        var fragments: Vector[Fragment[VType, UnDiEdge]] = fragmentComputation.compute(component, cycleGraph)

        /* When no more fragments are present, the entire component has been embedded. */
        while (fragments.nonEmpty) {
          val faces: Vector[Face[VType]] = faceComputation.computeFaces(embedding)
          val allAdmissibleFragmentsAndfaces = admissibleFaces(fragments, faces)

          /* If one or more fragments lack faces, they'll be at the front of the list (graph is non-planar).
           * If no such fragment exists, fragments with only one face take priority (again at the front of the list.
           *
           * Select one fragment, then embed an alpha path into the current embedding.
           */
          val fragment = allAdmissibleFragmentsAndfaces(0)._1
          val admFaces: Vector[Face[VType]] = allAdmissibleFragmentsAndfaces(0)._2

          if (admFaces.isEmpty) return None // <------- Not planar

          val face = admFaces(0)
          val alphaPath: Path[VType, UnDiEdge] = fragment.alphaPath
          embedding = pathEmbedder.embedPath(alphaPath, face, embedding)
          fragments = fragmentComputation.compute(component, GraphCommons.embeddingAsGraph(embedding))
        }

      } else {

        /* If no cycle exists then the component is a bridge, so every edge is embedded without regard to adjacency. */
        embedding = UndirectedEmbedding[VType]()

        for (edge <- component.edges) {
          embedding = embedding.embed(edge._1, edge._2)
        }

      }

      embeddings += embedding
      embeddingsToSubGraphs += (embedding -> component)
    }

    /* The graph is now guaranteed to be planar. Join all components in their contact vertices. */
    val finalResult = merger.merge(embeddingsToSubGraphs)

    Option(finalResult)
  }

  /**
   * Checks if a graph is planar in O(n2) time.
   * @param graph Graph to evaluate planarity for.
   * @return true if the graph is planar, otherwise false.
   */
  def isPlanar(graph: Graph[VType, EType]): Boolean = embed(graph).isDefined

  /**
   * Computes every admissible face for every fragment. The returned list is sorted so that the fragment with the
   * least amount of admissible faces occur first in the list.
   */
  private def admissibleFaces(fragments: Vector[Fragment[VType, UnDiEdge]], faces: Vector[Face[VType]]): Vector[(Fragment[VType, UnDiEdge], Vector[Face[VType]])] = {
    val result = new ArrayBuffer[(Fragment[VType, UnDiEdge], Vector[Face[VType]])]()

    for (fragment <- fragments) {
      val admissibleFaces = for {
        face <- faces
        if fragment.admissible(face)
      } yield face

      result += ((fragment, admissibleFaces))
    }

    result.toVector.sortWith(_._2.size < _._2.size)
  }

  private def makeUndirectedGraph(graph: Graph[VType, EType]): Graph[VType, UnDiEdge] = {
    val undiEdges = new ArrayBuffer[UnDiEdge[VType]]()
    for (edge <- graph.edges.toOuter) {
      undiEdges += (edge._1~edge._2)
    }

    Graph.from(graph.nodes.toOuter, undiEdges.toVector)
  }

}
