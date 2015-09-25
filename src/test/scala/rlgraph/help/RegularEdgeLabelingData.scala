package rlgraph.help

import net.cyndeline.rlgraph.embedding.Vertex
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.regularEdgeLabeling.RegularEdgeLabeling
import net.cyndeline.rlgraph.regularEdgeLabeling.angularMap.AngularMap

/**
 * Test data setups for regular edge labelings.
 */
object RegularEdgeLabelingData {

  /**
   * Contains an essential cycle 1, 2, 3, 4. Left-alternating.
   */
  def essentialFaceCycle = new {
    val north = 6
    val west = 5
    val south = 7
    val east = 8
    val totalEmbedding = UndirectedEmbedding()
      .embed(west, 1).embed(west, 4)
      .embed(1, north).embed(1, 2).embed(1, 3)
      .embed(2, north).embed(2, 3)
      .embedEdge(Vertex(1) withInsertPosition west inVertex 4 withInsertPosition 3)
      .embedEdge(Vertex(4) withInsertPosition 2 inVertex 3 withInsertPosition 1)
      .embedEdge(Vertex(east) withInsertPosition 2 inVertex 3 withDefaultInsertPosition)
      .embedEdge(Vertex(east) withInsertPosition north inVertex 2 withDefaultInsertPosition)
      .embedEdge(Vertex(south) withInsertPosition 3 inVertex 4 withDefaultInsertPosition)
      .embedEdge(Vertex(south) withInsertPosition east inVertex 3 withDefaultInsertPosition)
      // Outer edges
      .embedEdge(Vertex(west) withInsertPosition 1 inVertex north withInsertPosition 4)
      .embedEdge(Vertex(north) withInsertPosition 2 inVertex east withInsertPosition west)
      .embedEdge(Vertex(east) withInsertPosition 3 inVertex south withInsertPosition north)
      .embedEdge(Vertex(south) withInsertPosition 4 inVertex west withInsertPosition east)

    val t1 = totalEmbedding
      .deleteEdge(1, north).deleteEdge(2, north).deleteEdge(3, south).deleteEdge(4, south)
      .deleteEdge(1, 4).deleteEdge(2, 3)
      .embedEdge(Vertex(west) withInsertPosition north inVertex east withInsertPosition south)
    val t2 = totalEmbedding
      .deleteEdge(1, west).deleteEdge(4, west).deleteEdge(2, east).deleteEdge(3, east)
      .deleteEdge(1, 2).deleteEdge(1, 3).deleteEdge(4, 3)
      .embedEdge(Vertex(south) withInsertPosition west inVertex north withInsertPosition east)

    val edgesT1 = Vector((west, 1), (west, 4), (1, 2), (1, 3), (4, 3), (2, east), (3, east))
    val edgesT2 = Vector((1, north), (2, north), (4, 1), (3, 2), (south, 4), (south, 3))

    val labeling = RegularEdgeLabeling(edgesT1, edgesT2, totalEmbedding, t1, t2, north, west, south, east)
    val angularMap = new AngularMap(labeling)
  }

  /** Same as the first, except the inner edge goes between 2 and 4 instead of 1 and 3. This requires redirection
    * when flipping it. */
  def essentialFaceCycle2 = new {
    val f = essentialFaceCycle
    val north = f.north
    val west = f.west
    val south = f.south
    val east = f.east

    val newTotalEmbedding = f.totalEmbedding.deleteEdge(1, 3).embedEdge(Vertex(4) withInsertPosition 3 inVertex 2 withInsertPosition 1)

    val t2 = newTotalEmbedding
      .deleteEdge(1, north).deleteEdge(2, north).deleteEdge(3, south).deleteEdge(4, south)
      .deleteEdge(1, 4).deleteEdge(2, 3).deleteEdge(2, 4)
      .embedEdge(Vertex(west) withInsertPosition north inVertex east withInsertPosition south)
    val t1 = newTotalEmbedding
      .deleteEdge(1, west).deleteEdge(4, west).deleteEdge(2, east).deleteEdge(3, east)
      .deleteEdge(1, 2).deleteEdge(4, 3)
      .embedEdge(Vertex(south) withInsertPosition west inVertex north withInsertPosition east)

    val edgesT2 = Vector((west, 1), (west, 4), (1, 2), (4, 3), (2, east), (3, east))
    val edgesT1 = Vector((1, north), (2, north), (4, 1), (4, 2), (3, 2), (south, 4), (south, 3))

    val labeling = RegularEdgeLabeling(edgesT1, edgesT2, newTotalEmbedding, t1, t2, north, west, south, east)
    val angularMap = new AngularMap(labeling)
  }

  /**
   * An essential alternating 4-cycle 1, 2, 3, 4 that is represented by a cycle of length 8 in an angular map
   * derived from this REL. Clockwise.
   */
  def essentialEightCycleClockwise = new {
    val north = 8
    val west = 7
    val south = 10
    val east = 9

    val totalEmbedding = UndirectedEmbedding()
      .embed(west, 1).embed(west, 4)
      .embed(1, north).embed(1, 2).embed(1, 6).embed(1, 5)
      .embed(2, north).embed(2, 3)
      .embed(south, 4).embed(south, 3)
      .embedEdge(Vertex(1) withInsertPosition 7 inVertex 4 withInsertPosition 5)
      .embedEdge(Vertex(4) withInsertPosition south inVertex 3 withInsertPosition 1)
      .embedEdge(Vertex(east) withInsertPosition north inVertex 2 withDefaultInsertPosition)
      .embedEdge(Vertex(east) withInsertPosition 2 inVertex 3 withDefaultInsertPosition)
      .embedEdge(Vertex(3) withDefaultPositionInVertex 5 withInsertPosition 4)
      .embedEdge(Vertex(3) withDefaultPositionInVertex 6 withInsertPosition 5)
      .embedEdge(Vertex(4) withInsertPosition 3 inVertex 5 withInsertPosition 1)
      .embedEdge(Vertex(5) withInsertPosition 3 inVertex 6 withInsertPosition 1)
      .embedEdge(Vertex(6) withInsertPosition 3 inVertex 2 withInsertPosition 1)
      // Outer edges
      .embedEdge(Vertex(west) withInsertPosition 1 inVertex north withInsertPosition 4)
      .embedEdge(Vertex(north) withInsertPosition 2 inVertex east withInsertPosition west)
      .embedEdge(Vertex(east) withInsertPosition 3 inVertex south withInsertPosition north)
      .embedEdge(Vertex(south) withInsertPosition 4 inVertex west withInsertPosition east)

    val t1 = totalEmbedding
      .deleteEdge(1, north).deleteEdge(2, north).deleteEdge(3, south).deleteEdge(4, south)
      .deleteEdge(1, 4).deleteEdge(1, 5).deleteEdge(1, 6).deleteEdge(2, 3).deleteEdge(3, 5).deleteEdge(3, 6)
      .embedEdge(Vertex(west) withInsertPosition north inVertex east withInsertPosition south)
    val t2 = totalEmbedding
      .deleteEdge(1, west).deleteEdge(4, west).deleteEdge(2, east).deleteEdge(3, east)
      .deleteEdge(1, 2).deleteEdge(4, 3).deleteEdge(4, 5).deleteEdge(5, 6).deleteEdge(6, 2)
      .embedEdge(Vertex(south) withInsertPosition west inVertex north withInsertPosition east)

    val edgesT1 = Vector((west, 1), (west, 4), (1, 2), (4, 3), (2, east), (3, east), (4, 5), (5, 6), (6, 2))
    val edgesT2 = Vector((1, north), (2, north), (4, 1), (3, 2), (south, 4), (south, 3), (5, 1), (6, 1), (3, 5), (3, 6))

    val labeling = RegularEdgeLabeling(edgesT1, edgesT2, totalEmbedding, t1, t2, north, west, south, east)
    val angularMap = new AngularMap(labeling)
  }

  /**
   * The counter-clockwise version of the clockwise cycle.
   */
  def essentialEightCycleCounterClockwise = new {
    val f = essentialEightCycleClockwise
    val e_5_1 = f.labeling.edges.find(e => e.from == 5 && e.to == 1).get
    val e_6_1 = f.labeling.edges.find(e => e.from == 6 && e.to == 1).get
    val e_3_5 = f.labeling.edges.find(e => e.from == 3 && e.to == 5).get
    val e_3_6 = f.labeling.edges.find(e => e.from == 3 && e.to == 6).get
    val e_4_5 = f.labeling.edges.find(e => e.from == 4 && e.to == 5).get
    val e_5_6 = f.labeling.edges.find(e => e.from == 5 && e.to == 6).get
    val e_6_2 = f.labeling.edges.find(e => e.from == 6 && e.to == 2).get
    val labeling = f.labeling.flipEdges(Vector(e_5_1, e_6_1, e_3_5, e_3_6, e_4_5, e_5_6, e_6_2), Vector(true, true, true, true, false, false, false))
    val angularMap = new AngularMap(labeling)
  }

  /**
   * An 8-cycle [1, 2, 3, 4] to the left of a face-cycle [2, 7, 8, 3].
   */
  def adjacentFourCycles = new {
    val north = 10
    val west = 9
    val south = 12
    val east = 11

    val totalEmbedding = UndirectedEmbedding()
      .embed(west, 1).embed(west, 4)
      .embed(1, north).embed(1, 2).embed(1, 6).embed(1, 5)
      .embed(2, north).embed(2, 3)
      .embed(south, 4).embed(south, 3)
      .embedEdge(Vertex(1) withInsertPosition west inVertex 4 withInsertPosition 5)
      .embedEdge(Vertex(4) withDefaultPositionInVertex 5 withInsertPosition 1)
      .embedEdge(Vertex(5) withDefaultPositionInVertex 6 withInsertPosition 1)
      .embedEdge(Vertex(6) withInsertPosition 3 inVertex 2 withInsertPosition 1)
      .embedEdge(Vertex(4) withInsertPosition south inVertex 3 withInsertPosition 5)
      .embedEdge(Vertex(3) withInsertPosition 6 inVertex 5 withInsertPosition 4)
      .embedEdge(Vertex(3) withInsertPosition 2 inVertex 6 withInsertPosition 5)
      .embedEdge(Vertex(2) withDefaultPositionInVertex 7 withInsertPosition north)
      .embedEdge(Vertex(north) withDefaultPositionInVertex 7 withInsertPosition 1)
      .embedEdge(Vertex(3) withInsertPosition north inVertex 7 withInsertPosition 2)
      .embedEdge(Vertex(3) withDefaultPositionInVertex 8 withInsertPosition 7)
      .embedEdge(Vertex(south) withDefaultPositionInVertex 8 withInsertPosition 3)
      .embedEdge(Vertex(8) withInsertPosition north inVertex 7 withInsertPosition 3)
      .embedEdge(Vertex(7) withDefaultPositionInVertex east withInsertPosition north)
      .embedEdge(Vertex(8) withDefaultPositionInVertex east withInsertPosition 7)
      // Outer edges
      .embedEdge(Vertex(west) withInsertPosition 1 inVertex north withInsertPosition 4)
      .embedEdge(Vertex(north) withInsertPosition 7 inVertex east withInsertPosition west)
      .embedEdge(Vertex(east) withInsertPosition 8 inVertex south withInsertPosition north)
      .embedEdge(Vertex(south) withInsertPosition 4 inVertex west withInsertPosition east)

    val t1 = totalEmbedding
      .deleteEdge(1, north).deleteEdge(2, north).deleteEdge(7, north)
      .deleteEdge(1, 4).deleteEdge(1, 5).deleteEdge(1, 6).deleteEdge(5, 3).deleteEdge(6, 3)
      .deleteEdge(2, 3).deleteEdge(7, 3).deleteEdge(7, 8)
      .deleteEdge(4, south).deleteEdge(3, south).deleteEdge(8, south)
      .embedEdge(Vertex(west) withInsertPosition north inVertex east withInsertPosition south)

    val t2 = totalEmbedding
      .deleteEdge(1, west).deleteEdge(4, west).deleteEdge(1, 2).deleteEdge(4, 5).deleteEdge(5, 6).deleteEdge(6, 2)
      .deleteEdge(4, 3).deleteEdge(3, 8).deleteEdge(2, 7).deleteEdge(7, east).deleteEdge(8, east)
      .embedEdge(Vertex(south) withInsertPosition west inVertex north withInsertPosition east)

    val edgesT1 = Vector((west, 1), (west, 4), (1, 2), (4, 3), (4, 5), (5, 6), (6, 2), (2, 7), (3, 8), (7, east), (8, east))
    val edgesT2 = Vector((1, north), (2, north), (7, north), (4, 1), (3, 2), (south, 4), (south, 3), (south, 8), (5, 1), (6, 1), (3, 5), (3, 6), (3, 7), (8, 7))

    val labeling = RegularEdgeLabeling(edgesT1, edgesT2, totalEmbedding, t1, t2, north, west, south, east)
    val angularMap = new AngularMap(labeling)
  }

  /**
   * A face-cycle and an 8-cycle sharing an edge. Both cycles are left-alternating (moves cc).
   *
   * Note: N/S is switched.
   */
  def nonDisjointOppositeCycles = new {
    val north = 7
    val west = 8
    val south = 9
    val east = 10
    val totalEmbedding = UndirectedEmbedding()
      .embed(west, 6).embed(west, 4)
      .embed(6, 1).embed(6, 2).embed(6, 5).embed(6, 4)
      .embed(1, south).embed(1, east).embed(1, 3).embed(1, 2)
      .embed(5, 3).embed(5, north)
      .embedEdge(Vertex(south) withInsertPosition west inVertex 6 withInsertPosition 1)
      .embedEdge(Vertex(east) withInsertPosition 1 inVertex 3 withDefaultInsertPosition)
      .embedEdge(Vertex(2) withInsertPosition 5 inVertex 3 withInsertPosition 1)
      .embedEdge(Vertex(2) withInsertPosition 6 inVertex 5 withInsertPosition 3)
      .embedEdge(Vertex(4) withInsertPosition north inVertex 5 withInsertPosition 6)
      .embedEdge(Vertex(3) withInsertPosition 5 inVertex north withInsertPosition east)
      .embedEdge(Vertex(4) withInsertPosition 3 inVertex north withInsertPosition 5)
      // Outer edges
      .embedEdge(Vertex(west) withInsertPosition 6 inVertex south withInsertPosition 4)
      .embedEdge(Vertex(south) withInsertPosition 1 inVertex east withInsertPosition west)
      .embedEdge(Vertex(east) withInsertPosition 3 inVertex north withInsertPosition south)
      .embedEdge(Vertex(west) withInsertPosition east inVertex north withInsertPosition 4)

    val t1 = totalEmbedding
      .deleteEdge(1, south).deleteEdge(6, south).deleteEdge(6, 4).deleteEdge(1, 2)
      .deleteEdge(1, 3).deleteEdge(2, 5).deleteEdge(4, north).deleteEdge(3, north).deleteEdge(5, north)
      .embedEdge(Vertex(west) withInsertPosition south inVertex east withInsertPosition north)

    val t2 = totalEmbedding
      .deleteEdge(west, 6).deleteEdge(west, 4).deleteEdge(6, 1).deleteEdge(6, 2).deleteEdge(6, 5).deleteEdge(4, 5).deleteEdge(5, 3)
      .deleteEdge(2, 3).deleteEdge(1, east).deleteEdge(3, east)
      .embedEdge(Vertex(north) withInsertPosition west inVertex south withInsertPosition east)

    val edgesT1 = Vector((west, 6), (west, 4), (6, 1), (6, 5), (6, 2), (4, 5), (5, 3), (2, 3), (1, east), (3, east))
    val edgesT2 = Vector((south, 6), (south, 1), (6, 4), (2, 5), (1, 2), (1, 3), (3, north), (4, north), (5, north))

    val labeling = RegularEdgeLabeling(edgesT1, edgesT2, totalEmbedding, t1, t2, north, west, south, east)
    val angularMap = new AngularMap(labeling)
  }

}
