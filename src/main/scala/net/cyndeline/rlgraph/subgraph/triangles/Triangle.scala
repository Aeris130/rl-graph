package net.cyndeline.rlgraph.subgraph.triangles

import scala.reflect.ClassTag

/**
 * A cycle of three vertices in a graph.
 */
class Triangle[V : ClassTag] private (a: V, b: V, c: V) {

  val _1 = a
  val _2 = b
  val _3 = c

  val toVector: Vector[V] = Vector(a, b, c)
  def toArray: Array[V] = Array(a, b, c)

  override def equals(other: Any): Boolean = other match {
    case t: Triangle[V] => t._1 == a && t._2 == b & t._3 == c
    case _ => false
  }

  override val hashCode: Int = a.## ^ b.## ^ c.##

  override val toString: String = "Triangle<" + a + ", " + b + ", " + c + ">"
}

object Triangle {
  def apply[V : ClassTag](a: V, b: V, c: V) = new Triangle(a, b, c)
}
