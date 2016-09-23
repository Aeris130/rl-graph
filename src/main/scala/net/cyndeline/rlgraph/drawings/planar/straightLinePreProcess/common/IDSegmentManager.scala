package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.common

import scala.collection.mutable

/**
  * Stores and updates ID segments when moving them.
  *
  * @param rectangles Maps vertices against the id's of the lines representing their rectangles.
  */
class IDSegmentManager(val segments: Vector[IDLine],
                       val rectangles: Map[Int, (Int, Int, Int, Int)],
                       edgeId: Map[(Int, Int), Int]) {

  /**
    *
    * @param a A vertex.
    * @param b A vertex differing from a.
    * @return The id of the edge connecting a and b.
    */
  def edgeLine(a: Int, b: Int): Int = {
    require(a != b, "Cannot retrieve an edge id from two identical endpoints.")
    val start = Math.min(a, b)
    val stop = Math.max(a, b)
    edgeId(start, stop)
  }

  /**
    * @param edge An updated version of an edge in this manager.
    * @return A copy of this manager with the edge having the same id as the updated edge replaced by the updated one.
    */
  def updateEdge(edge: EdgeLine): IDSegmentManager = updateEdge(Vector(edge))
  def updateEdge(edges: Vector[EdgeLine]): IDSegmentManager = {
    val it = edges.iterator
    var current = segments
    while (it.hasNext) {
      val e = it.next()
      require(e.start.x.isWhole() && e.start.y.isWhole() && e.stop.x.isWhole() && e.stop.y.isWhole(), "Cannot perform update on segments with non-whole coordinates.")
      current = current.updated(e.id, e)
    }
    new IDSegmentManager(current, rectangles, edgeId)
  }

  /**
    * Adjusts the data of four border-edges, and disconnects their corners using a margin (adjusted edges stored
    * separately). Call this before updating edges, as the new border coordinates are needed when adjusting the
    * edge.
    *
    * @param a The updated version of one of the four border-edges of a vertex rectangle.
    * @return A copy of this manager with the edges having the same id as the updated edges replaced by the updated ones.
    */
  def updateBorder(a: VertexLine, b: VertexLine, c: VertexLine, d: VertexLine): IDSegmentManager = {
    assert(Vector(a.id, b.id, c.id, d.id).distinct.size == 4, "Duplicate border segments detected.")
    var originalSeg = segments
    for (o <- Vector(a, b, c, d))
      originalSeg = originalSeg.updated(o.id, o)

    new IDSegmentManager(originalSeg, rectangles, edgeId)
  }

  /**
    * @return True if the vertex v has four rectangle edges registered to it, false if its rectangle has length 1, and
    *         thus is not represented using edges.
    */
  def hasRectangle(v: Int): Boolean = rectangles.contains(v)

}

object IDSegmentManager {

  def apply(allSegments: Vector[IDLine]): IDSegmentManager = {
    var edgeId = Map[(Int, Int), Int]()
    val vertexToBorderSegments = new mutable.HashMap[Int, Vector[Int]]()
    val borders = allSegments.flatMap {
      case vl: VertexLine => Some(vl)
      case _ => None
    }
    for (b <- borders) {
      val current = vertexToBorderSegments.getOrElse(b.v, Vector())
      vertexToBorderSegments += (b.v -> (b.id +: current))
    }
    val rectangles = vertexToBorderSegments.toMap.map { kv =>
      assert(kv._2.size == 4, "Exactly four borders of a rectangle must be registered.")
      kv._1 -> (kv._2(0), kv._2(1), kv._2(2), kv._2(3))
    }
    val edges = allSegments.flatMap {
      case el: EdgeLine =>
        edgeId += (el.startVertex, el.stopVertex) -> el.id
        Some(el)
      case _ => None
    }.toIterator

    /* The same segment vector is used for both adjusted and unadjusted lines, they will be adjusted below before
     * returning it.
     */
    var unadjusted = new IDSegmentManager(allSegments, rectangles, edgeId)

    for (e <- edges) {
      unadjusted = unadjusted.updateEdge(e)
    }

    for (rect <- rectangles) {
      val a = vertexLine(rect._2._1, allSegments)
      val b = vertexLine(rect._2._2, allSegments)
      val c = vertexLine(rect._2._3, allSegments)
      val d = vertexLine(rect._2._4, allSegments)
      unadjusted = unadjusted.updateBorder(a, b, c, d)
    }

    unadjusted
  }

  private def vertexLine(id: Int, lines: Vector[IDLine]): VertexLine = lines(id) match {
    case vl: VertexLine => vl
    case _ => throw new Error(s"Attempted to retrieve ${lines(id)} as a vertex border.")
  }

}