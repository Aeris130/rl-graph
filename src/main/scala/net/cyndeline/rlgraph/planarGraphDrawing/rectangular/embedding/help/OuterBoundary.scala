package net.cyndeline.rlgraph.planarGraphDrawing.rectangular.embedding.help

import net.cyndeline.rlgraph.embedding.Embedding

import scala.collection.mutable.ArrayBuffer

/**
 * The vertices on the outside of the graph embedded so far.
 *
 * @param start A vertex in the embedding.
 * @param embedding An embedding with only 3 vertices connected in a triangle.
 */
class OuterBoundary[VType](start: VType, embedding: Embedding[VType]) {
  require(embedding.embeddedVertices.size == 3)

  // Increasing the index is equivalent to moving clockwise
  private val array: ArrayBuffer[VType] = setupBoundary
  private var pointer: Int = 0

  def moveClockwise(steps: Int) {
    for (i <- 0 until steps)
      pointer = nextIndex(pointer)
  }

  def moveCounterClockwise(steps: Int) {
    for (i <- 0 until steps)
      pointer = previousIndex(pointer)
  }

  def currentVertex: VType = array(pointer)

  def currentVertex(steps: Int): VType = {
    var p = pointer
    for (i <- 0 until steps)
      p = nextIndex(p)

    array(p)
  }

  def clockwiseVertex: VType = array(nextIndex(pointer))

  def clockwiseVertex(steps: Int): VType = {
    var p = pointer
    for (i <- 0 until steps)
      p = nextIndex(p)

    array(p)
  }

  def counterClockwiseVertex: VType = array(previousIndex(pointer))

  def counterClockwiseVertex(steps: Int): VType = {
    var p = pointer
    for (i <- 0 until steps)
      p = previousIndex(p)

    array(p)
  }

  def markAsCurrent(v: VType) {
    for (i <- 0 until array.size)
      if (array(i) == v) {
        pointer = i
        return
      }

    throw new Error("The vertex " + v + " was not found on the outer boundary.")
  }

  def remove(v: VType) {
    for (i <- 0 until array.size)
      if (array(i) == v) {
        array.remove(i)

        if (i <= pointer)
          pointer = previousIndex(pointer)

        return
      }
  }

  def add(v: VType, after: VType) {
    for (i <- 0 until array.size)
      if (array(i) == after) {
        array.insert(i + 1, v)
        if (i < pointer)
          pointer = nextIndex(pointer)

        return
      }
  }

  override def toString: String = {
    val builder = new StringBuilder()

    builder ++= "Outer boundary: "
    for (i <- 0 until array.size) {
      if (i == pointer)
        builder ++= "[" + array(i).toString + "]"
      else
        builder ++= array(i).toString

      if (i < array.size - 1)
        builder ++= ", "
    }

    builder.toString()
  }

  private def nextIndex(i: Int) = if (i == array.size - 1) 0 else i + 1
  private def previousIndex(i: Int) = if (i == 0) array.size - 1 else i - 1

  private def setupBoundary: ArrayBuffer[VType] = {
    val buffer = new ArrayBuffer[VType]()
    val startAdjacency = embedding.embeddingFor(start)
    var currentAdjacencyEntry = startAdjacency.head
    do {
      buffer += currentAdjacencyEntry.adjacentVertex
      currentAdjacencyEntry = currentAdjacencyEntry.next.moveTo
    } while (buffer.size < 3)

    buffer
  }
}
