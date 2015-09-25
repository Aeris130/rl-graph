package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.drawing

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.OrthogonalRepresentation

import scalax.collection.GraphEdge.UnDiEdge

/**
 * Specifies drawing coordinates for pairs of vertices (indicating an edge between them), as well as coordinates
 * for each bend between them.
 *
 * @constructor Constructs a new orthogonal drawing.
 * @param edges A triple representing an undirected edge. The first two values are the two vertices at each end of the
 *              edge, and the last is the coordinates of the edge. The first coordinate is for the first vertex, and
 *              every coordinate up until the last represents a bend that comes on the way to the second vertex.
 * @param vertices Every vertex in the drawing, mapped to the coordinate it should be drawn at.
 * @tparam VType Vertex type.
 */
class OrthogonalLayout[VType, EType[X] <: UnDiEdge[X]](val edges: Vector[DrawnEdge[VType, EType]], val vertices: Vector[(VType, Int, Int)])
  extends OrthogonalRepresentation[VType, EType] {

  /**
   * Constructs an empty orthogonal drawing.
   * @return An empty orthogonal drawing.
   */
  def this() = this(Vector(), Vector())

  override val hashCode: Int = edges.## ^ vertices.##

  override def equals(other: Any): Boolean = other match {
    case drawing: OrthogonalLayout[VType, EType] => this.## == drawing.## && drawing.edges == edges && drawing.vertices == vertices
    case _ => false
  }

  /**
   * Prints a neat description of each edge and its bends if any.
   */
  override def toString: String = {
    val sb = new StringBuilder()

    for (edge <- edges) {
      val from = edge.start
      val to = edge.stop

      sb ++= "<-- New edge -->\n"

      sb ++= "Start: [" + from.toString + "](" + edge.startPos + ")\n"

      for (bend <- edge.bends)
        sb ++= "Bend: " + bend + "\n"

      sb ++= "Stop: [" + to.toString + "](" + edge.stopPos + ")\n"
    }

    if (edges.isEmpty)
      for (vertex <- vertices)
        sb ++= vertex._1 + ": " + vertex._2 + "," + vertex._3

    sb.toString()
  }
}
