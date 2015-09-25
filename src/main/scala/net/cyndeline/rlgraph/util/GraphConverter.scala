package net.cyndeline.rlgraph.util

import scala.collection.mutable
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Converts a graph with edge classes that extend the undirected edge to a graph that uses undirected edges instead.
 *
 * Use the factory object to instantiate a converter.
 *
 * @param nodeFactory Converts nodes to a different type, if specified by the user. Defaults to a factory that returns
 *                    its input.
 * @param edgeFactory A method that takes an edge and the two converted vertices of the edge (in order _1, _2)
 *                    and produces a new edge from it.
 * @tparam V Type of vertex used in the input graph.
 * @tparam NV Vertex type produced by the user factory.
 * @tparam E Type of edge used in graph.
 * @tparam NE Type of edge produced by the user factory.
 */
class GraphConverter[V, NV : TypeTag, E[X] <: EdgeLikeIn[X], NE[X] <: EdgeLikeIn[X]: ({type l[M[_]] = TypeTag[M[NV]]})#l] private
  (nodeFactory: V => NV, edgeFactory: (E[V], NV, NV) => NE[NV]) extends GraphConverterI[V, NV, E, NE] {

  /**
   * @param graph Graph to convert.
   * @return A graph with the same topology as the input, but with nodes created by the factory, and
   *         edges being undirected.
   */
  def convert(graph: Graph[V, E]): Graph[NV, NE] = {
    val addedNodes = new mutable.HashMap[V, NV]()

    val nodes: Vector[NV] = (for {
      n <- graph.nodes
      outer: V = n
      converted: NV = convertNode(outer, addedNodes)
    } yield converted).toVector

    val edges: Vector[NE[NV]] = (for {
      edge <- graph.edges
      outerEdge = edge.toOuter
      nvFrom = getConvertedNode(outerEdge._1, addedNodes)
      nvTo = getConvertedNode(outerEdge._2, addedNodes)
      convertedEdge = edgeFactory(outerEdge, nvFrom, nvTo)
    } yield convertedEdge).toVector

    Graph.from(nodes, edges)
  }

  private def getConvertedNode(v: V, mapping: mutable.HashMap[V, NV]): NV = {
    mapping.get(v).getOrElse {
      convertNode(v, mapping)
    }
  }

  private def convertNode(v: V, mapping: mutable.HashMap[V, NV]): NV = {
    val converted = nodeFactory(v)
    mapping += v -> converted
    converted
  }

}

/**
 * Used for injection.
 */
trait GraphConverterI[V, NV, E[X] <: EdgeLikeIn[X], NE[X] <: EdgeLikeIn[X]] {
  def convert(graph: Graph[V, E]): Graph[NV, NE]
}

/**
 * Factory object that instantiates graph converters using method factories that produces new vertices and edges.
 */
object GraphConverter {

  /**
   * @tparam V Vertex type used in the input graph.
   * @tparam E Edge type used in the input graph.
   * @return A graph with it original vertices and every edge converted to undirected edges.
   */
  def toUndirected[V : TypeTag, E[X] <: EdgeLikeIn[X]: ({type l[M[_]] = TypeTag[M[V]]})#l]: GraphConverterI[V, V, E, UnDiEdge]
    = new GraphConverter[V, V, E, UnDiEdge]((v: V) => v, (e: E[V], v1: V, v2: V) => v1~v2)

  /**
   * Converts every vertex in a graph to a type supplied by the user, and every edge to an undirected edge.
   * @param nodeFactory A function that takes a vertex from the original graph and outputs another object. Will be
   *                    called once for every vertex in the graph, and only once per vertex.
   * @tparam V Vertex type used in the input graph.
   * @tparam NV Type that vertices in the original graph should be converted to.
   * @tparam E Edge type used in the input graph.
   * @return A graph converter that produces an undirected graph with vertices of the user-specified type.
   */
  def toUndirectedWithNodes[V : TypeTag, NV : TypeTag, E[X] <: EdgeLikeIn[X]: ({type l[M[_]] = TypeTag[M[NV]]})#l]
    (nodeFactory: V => NV): GraphConverterI[V, NV, E, UnDiEdge] = new GraphConverter[V, NV, E, UnDiEdge](nodeFactory, (e: E[V], v1: NV, v2: NV) => v1~v2)

  /**
   * Converts every edge in a graph, and not the vertices.
   * @param edgeFactory A method that takes an edge and the two converted vertices of the edge (in order _1, _2)
   *                    and produces a new edge from it.
   * @tparam V Vertex type used in the input graph.
   * @tparam E Edge type used in the input graph.
   * @tparam NE The edge type to convert the graph to.
   * @return A graph converter that produces a graph with the edge set converted by the user factory.
   */
  def convertEdges[V : TypeTag, E[X] <: EdgeLikeIn[X]: ({type l[M[_]] = TypeTag[M[V]]})#l, NE[X] <: EdgeLikeIn[X]: ({type l[M[_]] = TypeTag[M[V]]})#l]
    (edgeFactory: (E[V], V, V) => NE[V]): GraphConverterI[V, V, E, NE] = apply[V, V, E, NE]((v: V) => v, edgeFactory)

  /**
   * Converts a graph using a specified node factory.
   * @param nodeFactory A function that takes a vertex from the original graph and outputs another object. Will be
   *                    called once for every vertex in the graph, and only once per vertex.
   * @tparam V Vertex type used in the input graph.
   * @tparam NV Type that vertices in the original graph should be converted to.
   * @tparam E Edge type used in the input graph.
   * @tparam NE The edge type to convert the graph to.
   * @return A graph with its edges and vertices converted by the user-specified factories.
   */
  def apply[V : TypeTag, NV : TypeTag, E[X] <: EdgeLikeIn[X], NE[X] <: EdgeLikeIn[X]: ({type l[M[_]] = TypeTag[M[NV]]})#l]
    (nodeFactory: V => NV, edgeFactory: (E[V], NV, NV) => NE[NV]): GraphConverterI[V, NV, E, NE]
    = new GraphConverter[V, NV, E, NE](nodeFactory, edgeFactory)

}
