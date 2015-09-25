package net.cyndeline.rlgraph.util

import scalax.collection.GraphPredef.{EdgeLikeIn, OuterEdge}

/**
 * Creates a graph edge based on another edge by copying all data except to/from values.
 */
trait EdgeCopyFactory[VType, EType[X] <: EdgeLikeIn[X]] {

  /**
   * Copies an edge and assigns the copy new vertices.
   *
   * @param edge Old edge to copy.
   * @param a A vertex to assign the new edge. If the edge is directed, this is the .from value. For an undirected
   *          edge it is _1.
   * @param b A vertex to assign the new edge. If the edge is directed, this is the .to value. For an undirected
   *          edge it is _2.
   * @return a copy of the old edge but with new vertices assigned. Feeding the same triple (edge, a, b) into the
   *         factory multiple times should result in equal edges being produced.
   */
  def copyEdge(edge: EType[VType], a: VType, b: VType): EType[VType] with OuterEdge[VType, EType]

}
