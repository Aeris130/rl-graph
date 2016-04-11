package net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util

import scalax.collection.GraphEdge.{DiEdge, EdgeCopy, ExtendedKey, NodeProduct}
import scalax.collection.GraphPredef.OuterEdge

/**
 * Stores flow and cost of a residual edge.
 *
 * usage: implicit def edge2ResidualEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new ResidualEdgeAssoc[F](e)
 *
 * @constructor Creates a new residual edge.
 * @param id Unique identifier for this edge. The only value taken into consideration when comparing edges.
 * @param cost Cost of the residual edge.
 * @param flow Amount of flow currently being sent through the edge.
 * @param dir True if this edge is forward-pointing in the residual network (symbolizes that more flow may be sent
 *            along the edge this edge is representing), False if it is backwards pointing (allowing 'flow units
 *            of flow to be canceled)
 * @param owner The edge this residual edge is based on.
 */
class ResidualEdge[N](nodes: Product, val id: Int, val cost: Int, val flow: Int, val dir: Boolean, val owner: FlowEdge[FlowVertex])
  extends DiEdge[N](nodes)
  with ExtendedKey[N]
  with EdgeCopy[ResidualEdge]
  with OuterEdge[N, ResidualEdge]{

  def isInfinite = dir && flow < 0

  def hasInfiniteFLow = flow == -1

  def isBackwardEdge = !dir

  override def keyAttributes = Seq(id)

  override def copy[NN](newNodes: Product) = new ResidualEdge[NN](newNodes, id, cost, flow, dir, owner)

  override def equals(other: Any): Boolean = other match {
    case that: ResidualEdge[N] => that.id == id
    case _ => false
  }

  override def hashCode(): Int = id.##

  override def toString(): String = from + "~>" + to + "[f:" + flow + "|c:" + cost + (if (!dir) " <<" else "") + "]\n"
}

/*
 *
 * Needed by scalax.
 *
 */

object ResidualEdge {

  def apply(from: Int, to: Int, id: Int, cost: Int, flow: Int, dir: Boolean, owner: FlowEdge[FlowVertex]) =
    new ResidualEdge[Int](NodeProduct(from, to), id, cost, flow, dir, owner)

  def unapply(e: ResidualEdge[Int]): Option[(Int, Int, Int, Int, Int)] =
    if (e eq null) None else Some(e.from, e.to, e.id,e.cost, e.flow)
}

final class ResidualEdgeAssoc[F <: Int](val e: DiEdge[F]) {
  @inline def ##(id: Int, cost: Int, flow: Int, dir: Boolean, owner: FlowEdge[FlowVertex]): ResidualEdge[F] with OuterEdge[F, ResidualEdge] =
    new ResidualEdge[F](e.nodes, id, cost, flow, dir, owner) with OuterEdge[F, ResidualEdge]
}
