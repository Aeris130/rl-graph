package net.cyndeline.rlgraph.subgraph.isomorphism.jGraphT

import net.cyndeline.rlgraph.subgraph.isomorphism.{ElementEquivalence, IsomorphicMapping, NegativeCondition}
import net.cyndeline.rlgraph.util.graphConverters.jGraphT.ScalaGraphToJGraphTConverter

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Computes a mapping between the vertices of a graph and the vertices of the graph it is a sub graph of.
 *
 * @constructor Creates a new sub-graph inspector.
 */
//TODO check if typetags can be removed somehow
class SubGraphIsomorphismInspector extends IsomorphicMapping {
  private val inspector = new IsomorphismInspector()
  private val converter = new ScalaGraphToJGraphTConverter()

  /**
   * Checks if a graph contains a subset of vertices and edges that matches those of a sub graph, and returns
   * the first such topology that is found.
   *
   * @param subGraph Sub graph to check isomorphism for.
   * @param graph Graph to check if it contains the specified sub graph.
   * @return a mapping between vertices in the sub graph and the graph it is a sub graph of, if such is the case.
   *         Otherwise None.
   */
  override def firstIsomorphicMapping[VType : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l : ({type l[M[_]] = ClassTag[M[VType]]})#l](
                        subGraph: Graph[VType, EType],
                        graph: Graph[VType, EType],
                        comparator: ElementEquivalence[VType, EType],
                        negativeCondition: Option[NegativeCondition[VType, EType]]): Option[Map[VType, VType]] = {
    computeMapping(subGraph, graph, comparator, None, negativeCondition)
  }

  /**
   * Checks if a graph contains a subset of vertices and edges that matches those of a sub graph.
   *
   * @param subGraph Sub graph to check isomorphism for.
   * @param graph Graph to check if it contains the specified sub graph.
   * @param random If defined, selects a random topology of the graph matching the subgraph, instead of
   *               the first one to be found.
   * @return a mapping between vertices in the sub graph and the graph it is a sub graph of, if such is the case.
   *         Otherwise None.
   */
  override def randomIsomorphicMapping[VType : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l : ({type l[M[_]] = ClassTag[M[VType]]})#l](subGraph: Graph[VType, EType],
                        graph: Graph[VType, EType],
                        comparator: ElementEquivalence[VType, EType],
                        random: Random,
                        negativeCondition: Option[NegativeCondition[VType, EType]]): Option[Map[VType, VType]] = {
    computeMapping(subGraph, graph, comparator, Some(random), negativeCondition)
  }

  /**
   * Checks isomorphism.
   */
  private def computeMapping[VType : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l : ({type l[M[_]] = ClassTag[M[VType]]})#l](subGraph: Graph[VType, EType],
                             graph: Graph[VType, EType],
                             comparator: ElementEquivalence[VType, EType],
                             random: Option[Random],
                             negativeCondition: Option[NegativeCondition[VType, EType]]): Option[Map[VType, VType]] = {
    if (subGraph.isEmpty || subGraph.nodes.size > graph.nodes.size || subGraph.edges.size > graph.edges.size)
      return None

    val jGraphTSubGraph = converter.convert(subGraph)
    val allVertices = graph.nodes.toOuter.toSet[VType].toVector

    /* Every vertex combination of the same size as the sub graph (no repetitions since no vertex occurs
     * twice in a graph).
     */
    val vertexCombinations: Vector[Vector[VType]] = allVertices.combinations(subGraph.nodes.size).toVector
    val shuffledVertexLists: Vector[Vector[VType]] =
      if (random.isDefined)
        random.get.shuffle(vertexCombinations)
      else
        vertexCombinations

    /* Try each combination until a result is found. */
    val vertexLists = shuffledVertexLists.iterator
    while (vertexLists.hasNext) {
      val vertexList: Vector[VType] = vertexLists.next()

      val graphWithoutEdges = Graph.from[VType, EType](vertexList, Nil)

      /* A graph with very vertex in the list, and every edge connected to those vertices. */
      val subsetBasedOnList: Graph[VType, EType] = graph diff (graph diff graphWithoutEdges)

      /* The graph can still contain additional edges not found in the sub graph, if both share the same vertices. */
      val allEdges = subsetBasedOnList.edges.toOuter.toSet[EType[VType]].toVector
      val edgeCombinations = allEdges.combinations(subGraph.edges.size) // And Bachmann wept ;_;
      while (edgeCombinations.hasNext) {
        val edges: Vector[EType[VType]] = edgeCombinations.next()
        val graphWithEdgeSubset = Graph.from(vertexList, edges)

        val isomorphicResult: Option[Map[VType, VType]] = inspector.isomorphicMapping[VType, EType](jGraphTSubGraph, graphWithEdgeSubset, subGraph, comparator, negativeCondition, graph)

        if (isomorphicResult.isDefined)
          return isomorphicResult

      }
    }

    None
  }

}
