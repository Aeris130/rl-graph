package net.cyndeline.rlgraph.regularEdgeLabeling

import net.cyndeline.rlgraph.embedding.Embedding

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
 * Computes the two sets of edges, T1 T2, associated with a regular edge labeling such that for each interior
 * vertex v the edges incident to v appear in counterclockwise order around v as follows: A set of edges in T1 leaving
 * v, a set of edges in T2 entering v, a set of edges in T1 entering v and a set of edges in T2 leaving v.
 */
trait RELFactory {

  /**
   * @param embedding A planar inner triangulated embedding of a graph, with the outer face being a quadrangle having
   *                  vertices in the order South -> West -> North -> East.
   * @param north The northern outer face vertex.
   * @param south The southern outer face vertex.
   * @param west The western outer face vertex.
   * @param east The eastern outer face vertex.
   * @return A regular edge labeling for the embedding.
   */
  def produceRegularEdgeLabeling[V : TypeTag : ClassTag](embedding: Embedding[V], north: V, south: V, west: V, east: V): EdgeLabeling[V]

}
