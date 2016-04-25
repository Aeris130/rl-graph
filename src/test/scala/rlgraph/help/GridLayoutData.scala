package rlgraph.help

import net.cyndeline.rlgraph.embedding.Vertex
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding

import scala.language.postfixOps

/**
  * Data used in testing objects related to planar grid layouts.
  */
object GridLayoutData {

  /**
    * A general embedding.
    */
  def embedding = new {
    val e = UndirectedEmbedding[Int]().embed(1, 2) // Start edge
      .embed(1, 3).embed(3, 4).embed(4, 5).embed(5, 2) // first row
      // Second row
      .embedEdge(Vertex(6) withInsertPosition 2 inVertex 1 withDefaultInsertPosition)
      .embedEdge(Vertex(7) withInsertPosition 1 inVertex 6 withDefaultInsertPosition)
      .embedEdge(Vertex(8) withInsertPosition 6 inVertex 7 withDefaultInsertPosition)
      .embedEdge(Vertex(5) withInsertPosition 7 inVertex 8 withInsertPosition 4)
      // Edges between first and second row
      .embedEdge(Vertex(3) withInsertPosition 8 inVertex 7 withInsertPosition 1)
      .embedEdge(Vertex(4) withInsertPosition 8 inVertex 7 withInsertPosition 3)
      .embedEdge(Vertex(4) withInsertPosition 5 inVertex 8 withInsertPosition 7)
      .embedEdge(Vertex(5) withInsertPosition 7 inVertex 8 withInsertPosition 4)
      // Third row
      .embedEdge(Vertex(9) withInsertPosition 6 inVertex 7 withDefaultInsertPosition)
      .embedEdge(Vertex(10) withDefaultPositionInVertex 9 withDefaultInsertPosition)
      .embedEdge(Vertex(8) withInsertPosition 9 inVertex 10 withInsertPosition 7)
      // Edge from 2 to row 2
      .embedEdge(Vertex(8) withInsertPosition 5 inVertex 2 withInsertPosition 10)
  }

  /**
    * An edge 1-2, then a single row 1-3-4-5-2.
    */
  def singleRow = new {
    val e = UndirectedEmbedding[Int]().embed(1, 2) // Start edge
      .embed(1, 3).embed(3, 4).embed(4, 5).embed(5, 2) // first row
  }

}
