package net.cyndeline.rlgraph.planarGraphDrawing.rectangular.dataStructure

/**
 * A rectangular area that results from splitting an edge to remove separating triangles from the original graph.
 * Because of this, this area joins the vertices originally connected via the edge, allowing "travel" from one of
 * the edge's vertices through the gate and to the other vertex, as the gate will always be adjacent to both.
 *
 * @param from One of the vertices connected via the gate.
 * @param to The other vertex connected to the first.
 * @param startX Starting x coordinate of the rectangle (inclusive).
 * @param startY Starting y coordinate of the rectangle (inclusive).
 * @param stopX Stop x coordinate of the rectangle (inclusive).
 * @param stopY Stop y coordinate of the rectangle (inclusive).
 */
class Gate[VType](val from: VType, val to: VType, startX: Int, startY: Int, stopX: Int, stopY: Int) extends Rectangle(startX, startY, stopX, stopY, false) {

  override def isVertex: Boolean = false
  override def isGate: Boolean = true

  override def equals(other: Any): Boolean = other match {
    case g: Gate[VType] => from == g.from && to == g.to && startX == g.startX && startY == g.startY && stopX == g.stopX && stopY == g.stopY
    case _ => false
  }

  override val hashCode: Int = from.## ^ to.## ^ startX.## ^ stopX.## ^ startY.## ^ stopY.##
}

object Gate {
  def apply[VType](from: VType, to: VType, startX: Int, startY: Int, stopX: Int, stopY: Int) = new Gate(from, to, startX, startY, stopX, stopY)
}
