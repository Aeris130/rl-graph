package net.cyndeline.rlgraph.embedding

/**
 * Initial factory object, used to initiate a miniature DSL that makes edge embedding with positions a bit easier
 * to read and write.
 *
 * Usage: Assuming you're trying to insert an edge 1~2, with vertex 1 being inserted at position X in vertex 2, and
 * vertex 2 being inserted at position Y in vertex 1.
 *
 * Vertex(1) withInsertPosition X inVertex 2 withInsertPosition Y
 *
 * No insert position for 1:
 *
 * Vertex(1) withDefaultPositionInVertex 2 withInsertPosition Y
 */
object Vertex {
  def apply[T](v: T): AwaitingFirstPosition[T] = new AwaitingFirstPosition(v)
}

/**
 * Specifies the insert position for vertex A in vertex B.
 */
class AwaitingFirstPosition[T](val insertedVertex: T) {
  def withInsertPosition(p: T): SpecifySecondVertex[T] = new SpecifySecondVertex(insertedVertex, p)
  def withDefaultPositionInVertex(v: T): AwaitingSecondPosition[T] = new AwaitingSecondPosition(insertedVertex, None, v)
}

/**
 * Specifies the second vertex to insert.
 */
class SpecifySecondVertex[T](val firstVertex: T, val firstInsertionPosition: Option[T]) {
  def this(insert: T, insertPos: T) = this(insert, Some(insertPos))
  def this(insert: T) = this(insert, None)

  def inVertex(v: T) = new AwaitingSecondPosition(firstVertex, firstInsertionPosition, v)
}

/**
 * Specifies the insert position for vertex B in vertex A.
 */
class AwaitingSecondPosition[T](val firstVertex: T, val firstInsertionPosition: Option[T], secondVertex: T) {
  def withInsertPosition(p: T): EdgeInsert[T] = new EdgeInsert(firstVertex, firstInsertionPosition, secondVertex, Some(p))
  def withDefaultInsertPosition: EdgeInsert[T] = new EdgeInsert(firstVertex, firstInsertionPosition, secondVertex, None)
}

class EdgeInsert[T](val firstVertexToInsert: T, val positionToInsertInSecondVertex: Option[T], val secondVertexToInsert: T, val positionToInsertInFirstVertex: Option[T])