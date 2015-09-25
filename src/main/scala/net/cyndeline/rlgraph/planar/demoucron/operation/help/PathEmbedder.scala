package net.cyndeline.rlgraph.planar.demoucron.operation.help

import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.pathfinding.Path

import scalax.collection.GraphEdge.UnDiEdge

/**
 * Embeds cycles and paths inside a face. The idea is that an embedding were edges are stored
 * counter clockwise around each vertex, and faces with clockwise orientation are computed by
 * moving to the previous edge from any given start point on a adjacency list, can place a path
 * inside the face by connecting the edges of the path onto the face positioned after the node entry
 * of the vertex after the vertex to connect to.
 *
 * Example: The face 1, 2, 3, 4, 5 (5 is connected to 1)
 *
 * In such a face, vertex 3 stores its edges in the order 4, 2. Assuming 3 also connects to another vertex outside
 * the face (example: 6), the order would be 4, 2, 6. Adding a path 1, 7, ..., 8, 3 to this face involves connectng
 * vertex 7 and 8 to 1 and 3. In vertex 3's adjacency list, this means placing it after 4 (4, 8, 2, 6) since anything
 * between 4 and 2 counts as inside the face.
 *
 * @constructor Creates a new path embedder.
 */
class PathEmbedder[VType] {

  /**
   * Embeds a path inside a face.
   *
   * @param path Path to embed.
   * @param face Face to embed into.
   * @param embedding Current embedding containing the first and last vertex on the path.
   * @return A new embedding with the path embedded into the specified face.
   */
  def embedPath(path: Path[VType, UnDiEdge], face: Face[VType], embedding: Embedding[VType]): Embedding[VType] = {
    val nodes = path.vertices
    val first = nodes.head
    val last = nodes.last
    val insertFirstAfter = findInsertEntryStart(nodes.head, (face.vertices :+ face.vertices.head).toList)
    val insertLastAfter = findInsertEntryStart(nodes.last, (face.vertices :+ face.vertices.head).toList)
    val nonContactVertices = nodes diff Vector(first, last)

    if (nonContactVertices.isEmpty) {
      embedding.embed(first, last, Option(insertLastAfter), Option(insertFirstAfter))

    } else {
      val vertices = nonContactVertices.iterator
      var emb = embedding
      var current = first
      var next = vertices.next()

      emb = emb.embed(current, next, None, Option(insertFirstAfter))
      current = next

      while (vertices.hasNext) {
        next = vertices.next()
        emb = emb.embed(current, next)
        current = next
      }

      emb = emb.embed(current, last, Option(insertLastAfter), None)
      emb
    }
  }

  /**
   * Embeds a cycle into an embedding with no regard to edge order, since every vertex adjacency list
   * only have two entries with the other entry set as both next and previous. No previous vertex on the cycle may
   * exist prior to embedding.
   *
   * @param cycle A cycle to embed.
   * @param embedding Embedding to embed cycle into.
   * @return A new embedding with the cycle embedded.
   */
  def embedCycle(cycle: Vector[VType], embedding: Embedding[VType]): Embedding[VType] = {
    if (cycle.head != cycle.last) throw new IllegalArgumentException("The cycle " + cycle + " did not contain the same element first and last in the list.")
    else if (cycle.size < 3) throw new IllegalArgumentException("The cycle " + cycle + " did not contain at least three elements.")

    for (vertex <- cycle) if (embedding.isEmbedded(vertex)) {
      throw new IllegalArgumentException("Vertex " + vertex + " in cycle " + cycle + " existed in embedding prior to cycle embedding.")
    }

    cycleEmbed(cycle.toList, embedding)
  }

  /*
   * The actual cycle embedding.
   */
  private def cycleEmbed(cycle: List[VType], embedding: Embedding[VType]): Embedding[VType] = cycle match {
    case Nil => throw new IllegalArgumentException("Can't embed an empty cycle.")
    case head::Nil => embedding
    case head::tail => cycleEmbed(tail, embedding.embed(head, tail.head))
  }

  /**
   * Given a clockwise face computed by traversing the opposite direction of a counter clockwise face,
   * finds the edge entry to insert a new edge after so that the new edge is placed inside the face.
   *
   *
   *
   * @param node Node to insert edge onto.
   * @param face Face that the node belongs to.
   * @return The vertex whose edge the new edge should be embedded counter clockwise after.
   */
  private def findInsertEntryStart(node: VType, face: List[VType]): VType = {
    face match {
      case entry::Nil => entry
      case entry::rest => if (entry == node) rest(0) else findInsertEntryStart(node, rest)
    }
  }
}
