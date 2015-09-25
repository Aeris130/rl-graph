package net.cyndeline.rlgraph.canonicalOrder.planar4ConnectedTriangular.help

import scala.collection.mutable

/**
 * Stores the visitation status and number of chords adjacent to every vertex on the outer face cycle C(k).
 */
class VertexLabels[V](vs: Vector[V]) {
  private val labelMap = new mutable.HashMap[V, Label]()
  for (v <- vs)
    labelMap += v -> new Label()

  private class Label {
    var visited: Int = 0
    var chords: Int = 0
  }

  def vertices: Vector[V] = labelMap.keys.toVector

  def deRegisterVertex(v: V) {
    labelMap -= v
  }

  def chords(v: V): Int = labelMap(v).chords

  def visits(v: V): Int = labelMap(v).visited

  def isVisited(v: V): Boolean = labelMap(v).visited > 0

  def visit(v: V) {
    labelMap(v).visited += 1
  }

  def addChord(v: V) {
    labelMap(v).chords += 1
  }

  def removeChord(v: V) {
    labelMap(v).chords = labelMap(v).chords - 1
  }

  override def toString: String = {
    val builder = new StringBuilder()
    val nl = System.getProperty("line.separator")

    for (kv <- labelMap) {
      builder ++= kv._1 + " Visits: " + kv._2.visited + ", chords: " + kv._2.chords + nl + "----------" + nl
    }

    builder.toString()
  }
}
