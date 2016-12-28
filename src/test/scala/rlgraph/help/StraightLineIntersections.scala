package rlgraph.help

import net.cyndeline.rlcommon.math.geom.Line
import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
  * Computes intersections in a straight-line drawing, except for the ones resulting from neighbors intersecting
  * at a common vertex.
  */
object StraightLineIntersections extends SpecImports {

  def validateDrawing(drawing: StraightLineDrawing[Int], graph: Graph[Int, UnDiEdge]): Unit = {
    assert(drawing.coordinates.values.toSet.size == drawing.vertices.size, "Overlapping coordinates found")

    for (edge1 <- drawing.edges; edge2 <- drawing.edges if UnorderedPair(edge1) != UnorderedPair(edge2)) {
      val l1 = Line(drawing.coordinates(edge1._1), drawing.coordinates(edge1._2))
      val l2 = Line(drawing.coordinates(edge2._1), drawing.coordinates(edge2._2))
      val intersection = l1.intersection(l2)
      if (intersection.isDefined && (intersection.get.isInterval || (Set(edge1._1, edge1._2) intersect Set(edge2._1, edge2._2)).isEmpty))
        fail(s"Edge intersection detected between $edge1 and $edge2")
    }


  }

}
