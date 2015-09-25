package net.cyndeline.rlgraph.subgraph.triangles

/**
 * Used as vertices when looking for triangles. Can be marked to signal that the vertex has been processed.
 */
class Mark[V] private (val markValue: V) {
  private var m = false

  def mark(): Unit = m = true
  def unMark(): Unit = m = false
  def isMarked: Boolean = m

  override val toString: String = "Mark[" + markValue + ", marked: " + m + "]"

  override def equals(other: Any): Boolean = other match {
    case m: Mark[V] => markValue == m.markValue && isMarked == m.isMarked
    case _ => false
  }

  override val hashCode: Int = markValue.##
}

object Mark {
  def apply[V](markValue: V) = new Mark(markValue)
}
