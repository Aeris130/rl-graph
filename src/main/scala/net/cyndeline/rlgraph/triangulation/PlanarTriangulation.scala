package net.cyndeline.rlgraph.triangulation

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Specifies methods used when triangulating a planar graph.
 */
trait PlanarTriangulation {

  /**
   * Computes the edges needed to be added to a graph in order to triangulate it.
   * @param graph A planar graph to triangulate. Can be disconnected.
   */
  def triangulate[VType: TypeTag : ClassTag](graph: Graph[VType, UnDiEdge]): Option[TriangulationEdges[VType]]

}
