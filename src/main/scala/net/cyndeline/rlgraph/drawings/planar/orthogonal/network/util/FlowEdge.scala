package net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._

/**
 * Extends scalax graph edges in order to create a graph-representation of a flow network
 * without having to store flow data as labels (improves readability).
 *
 * Implicit usage:
 *  implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
 *
 * @constructor Creates a new flow edge.
 * @param nodes Filled in by scalax's graph classes. Don't instance this class yourself.
 * @param id Unique id for this edge. Needed to allow the graph to have more than one edge going between a node pair.
 * @param lowerBound The lowest flow value possible.
 * @param capacity The highest flow value possible. -1 if the capacity is infinite.
 * @param cost The cost for sending 1 unit of flow through the edge.
 * @param flow The amount of flow going through the edge.
 * @param crosses If this edge goes from a face-vertex to another face-vertex, this value represents the edge on the original
 *          graph the edge passes through, stored as the vertex pair of the edge in the order they are traversed
 *          in the face. None for edges going from a regular vertex to a face-vertex.
 */
class FlowEdge[N](nodes: Product, val id: Int, val lowerBound: Int, val capacity: Int, val cost: Int, val flow: Int, val crosses: Option[(_, _)])
  extends DiEdge[N](nodes)
  with ExtendedKey[N]
  with EdgeCopy[FlowEdge]
  with OuterEdge[N, FlowEdge] {
  if (capacity > 0 && capacity < lowerBound) throw new IllegalArgumentException("Lowerbound " + lowerBound + " is higher than capacity " + capacity)
  if (capacity > 0 && flow > capacity) throw new IllegalArgumentException("Flow " + flow + " is higher than capacity " + capacity)

  def hasInfiniteCapacity: Boolean = capacity < 0

  override def copy[NN](newNodes: Product) = new FlowEdge[NN](newNodes, id, lowerBound, capacity, cost, flow, crosses)

  def keyAttributes = Seq(id)

  override def equals(other: Any): Boolean = other match {
    case that: FlowEdge[N] => that.id == id
    case _ => false
  }

  override def hashCode(): Int = id.##

  override def toString(): String = {
    val crossStr = if (crosses.isDefined) "|cross:" + crosses.get.toString else ""
    val lowerBoundStr = if (lowerBound > 0) "|bound:" + lowerBound.toString else ""
    val costStr = if (cost > 0) "|cost:" + cost.toString else ""
    "<" + id + ">" + from + "~>" + to + "[f:" + flow + "|cap:" + capacity + crossStr + lowerBoundStr + costStr + "]\n"
  }
}

/*
 *
 * Needed by scalax.
 *
 */

object FlowEdge {

  def apply(from: FlowVertex, to: FlowVertex, id: Int, lowerBound: Int, capacity: Int, cost: Int, flow: Int, crosses: Option[(_, _)]) =
    new FlowEdge[FlowVertex](NodeProduct(from, to), id, lowerBound, capacity, cost, flow, crosses)

  def unapply(e: FlowEdge[FlowVertex]): Option[(FlowVertex, FlowVertex, Int, Int, Int, Int, Int, Option[(_, _)])] =
    if (e eq null) None else Some(e.from, e.to, e.id, e.lowerBound, e.capacity, e.cost, e.flow, e.crosses)
}

final class FlowEdgeAssoc[F <: FlowVertex](val e: DiEdge[F]) {

  @inline def ##(id: Int, lowerBound: Int, capacity: Int, cost: Int, flow: Int, crosses: Option[(_, _)]): FlowEdge[F] with OuterEdge[F, FlowEdge] =
    new FlowEdge[F](e.nodes, id, lowerBound, capacity, cost, flow, crosses) with OuterEdge[F, FlowEdge]

  @inline def ##(id: Int, lowerBound: Int, capacity: Int, cost: Int, flow: Int): FlowEdge[F] with OuterEdge[F, FlowEdge] = ##(id, lowerBound, capacity, cost, flow, None)
}
