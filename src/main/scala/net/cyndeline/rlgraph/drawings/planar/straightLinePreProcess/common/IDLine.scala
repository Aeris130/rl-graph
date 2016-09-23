package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.common

import net.cyndeline.rlcommon.math.geom.{Line, RPoint, Rectangle}
import spire.math.Rational

private[straightLinePreProcess] abstract class IDLine(val id: Int, from: RPoint, to: RPoint) extends Line(from, to) {
  def isEdge: Boolean
  def isVertex: Boolean
}

/**
  * Associates a line segment with vertices for its start- and stop coordinates.
  */
private[straightLinePreProcess] class EdgeLine(id: Int, val startVertex: Int, val stopVertex: Int, startC: RPoint, stopC: RPoint) extends IDLine(id, startC, stopC) {
  require(startVertex != stopVertex, s"Vertex $startVertex was specified as both start and stop in a line segment from $startC to $stopC")
  require(startVertex < stopVertex, "The start vertex must have a lower id than a stop vertex in edge lines.")
  require(startC != stopC, "Cannot begin and end edge line at same coordinate.")

  override def isEdge: Boolean = true
  override def isVertex: Boolean = false

  def coordinateOf(v: Int): RPoint = {
    assert(v == startVertex || v == stopVertex)
    if (v == startVertex) startC
    else if (v == stopVertex) stopC
    else throw new Error(s"The vertex $v is not a member of $this")
  }

  def adjust(v: Int, newCoordinate: RPoint) = {
    val newStartC = if (v == startVertex) newCoordinate else startC
    val newStopC = if (v == stopVertex) newCoordinate else stopC
    EdgeLine(id, startVertex, stopVertex, newStartC, newStopC)
  }

  override def toString: String = s"E($startVertex->$stopVertex, (${start.x}, ${start.y})->(${stop.x}, ${stop.y})"
  override def hashCode: Int = startVertex.## ^ stopVertex.## ^ startC.## ^ stopC.##
  override def equals(other: Any): Boolean = other match {
    case e: EdgeLine => startVertex == e.startVertex && stopVertex == e.stopVertex && start == e.start && stop == e.stop
    case _ => false
  }

}

private[straightLinePreProcess] object EdgeLine {
  def apply(id: Int, startVertex: Int, stopVertex: Int, startC: RPoint, stopC: RPoint) = {
    val start = Math.min(startVertex, stopVertex)
    val stop = Math.max(startVertex, stopVertex)
    val startCoordinate = if (start == startVertex) startC else stopC
    val stopCoordinate = if (stop == stopVertex) stopC else startC
    new EdgeLine(id, start, stop, startCoordinate, stopCoordinate)
  }
}

private[straightLinePreProcess] class VertexLine(id: Int, val v: Int, val side: VSide, from: RPoint, to: RPoint) extends IDLine(id, from, to) {
  override def isEdge: Boolean = false
  override def isVertex: Boolean = true

  def adjustBy(x: Rational, y: Rational) = new VertexLine(id, v, side, from + (x, y), to + (x, y))

  override def toString: String = s"V($v, $side, (${from.x}, ${from.y})->(${to.x}, ${to.y})"
  override def equals(other: Any): Boolean = other match {
    case vl: VertexLine => v == vl.v && side == vl.side && start == vl.start && stop == vl.stop
    case _ => false
  }
  override def hashCode: Int = v.## ^ side.## ^ from.## ^ to.##
}

private[straightLinePreProcess] object VertexLine {
  def apply(v: Int, r: Rectangle, nextId: Int): Vector[VertexLine] = {
    val start = r.start
    val stop = r.stop
    Vector (
      new VertexLine(nextId, v, Left, start, RPoint(start.x, stop.y)),
      new VertexLine(nextId + 1, v, Bottom, RPoint(start.x, start.y), RPoint(stop.x, start.y)),
      new VertexLine(nextId + 2, v, Right, RPoint(stop.x, start.y), stop),
      new VertexLine(nextId + 3, v, Top, RPoint(start.x, stop.y), stop)
    )
  }
}

private[common] sealed trait VSide { def id: Int }
private[common] case object Left extends VSide { val id: Int = 0 }
private[common] case object Bottom extends VSide { val id: Int = 1 }
private[common] case object Right extends VSide { val id: Int = 2 }
private[common] case object Top extends VSide { val id: Int = 3 }
