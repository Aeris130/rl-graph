package net.cyndeline.rlgraph.planarGraphDrawing.rectangular.triangleBreak.help

/**
 * A bubble with 3 vertices in it, representing the 3 edges of a separating triangle.
 *
 * Increases the weight of its vertices upon instantiation.
 */
class Bubble[V](val v1: BubbleVertex[V], val v2: BubbleVertex[V], val v3: BubbleVertex[V]) {
  v1.addWeight(1)
  v2.addWeight(1)
  v3.addWeight(1)

  /** @return The total weight of all vertices in this bubble. */
  def weight: Int = v1.weight + v2.weight + v3.weight

  /** @return The vertex with the highest weight. */
  def highestWeightVertex: BubbleVertex[V] = Vector(v1, v2, v3).maxBy(_.weight)

  /** De-registers this bubble in all its vertices and decreases their weight by 1. */
  def pop() {
    v1.removeWeight(1)
    v2.removeWeight(1)
    v3.removeWeight(1)
  }

  /** @return The k value for this bubble (i.e the number of vertices with weight > 1). */
  def k: Int = vertices.count(_.weight > 1)

  val vertices: Vector[BubbleVertex[V]] = Vector(v1, v2, v3)

  override def equals(other: Any): Boolean = other match {
    case b: Bubble[V] => v1 == b.v1 && v2 == b.v2 && v3 == b.v3
    case _ => false
  }

  override val hashCode: Int = v1.## ^ v2.## ^ v3.##

  override def toString: String = "Bubble(" + v1 + ", " + v2 + ", " + v3 + ")"
}
