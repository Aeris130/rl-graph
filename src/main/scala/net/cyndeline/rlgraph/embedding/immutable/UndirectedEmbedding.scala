package net.cyndeline.rlgraph.embedding.immutable

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.embedding.{AdjacencyList, EdgeInsert, Embedding}

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Creates a new immutable undirected embedding.
 */
object UndirectedEmbedding {
  def apply[VType]() = new UndirectedEmbedding[VType]()
}

/**
 * Stores the circular order of edges around each vertex in a graph. Use the factory object to create the initial
 * empty embedding.
 *
 * @param vertexIndices Maps each vertex in the embedding to the integer index in the entry vector. Used when
 *                      looking up a vertex externally.
 * @param adjacencyPositions Keeps track of where in its neighbors adjacency vectors a vertex is stored. The first key is
 *                           the index of a vertex. The value is another map where the key is the index of its
 *                           neighbor, and the value if the position in the neighbors adjacency vector that the
 *                           vertex is stored.
 * @param size The number of vertex entries.
 * @tparam VType Type of nodes used in the graph being embedded.
 */
class UndirectedEmbedding[VType] private (val vertexIndices: Map[VType, Int],
                                          val adjacencyLists: Vector[(VType, Vector[Int])],
                                          val adjacencyPositions: Map[Int, Map[Int, Int]],
                                          size: Int) extends Embedding[VType] {

  private def this() = this(Map(), Vector(), Map(), 0)

  /**
   * Checks if the embedding contains any vertices (a prerequisite for containing edges,
   * and vice versa).
   * @return True if the embedding has at least one edge embedded, otherwise false.
   */
  override def isEmpty: Boolean = vertexIndices.isEmpty

  /**
   * @param vertex Vertex to check membership for.
   * @return True if the vertex is embedded, otherwise false.
   */
  def isEmbedded(vertex: VType): Boolean = vertexIndices.contains(vertex)

  /**
   * Retrieves the head element in the list of edges connected to a single vertex in a graph. Note that this
   * element is arbitrarily chosen as Head, as the list is circular.
   *
   * @param vertex The vertex to retrieve an embedding for.
   * @return the head of an adjacency list for the specified vertex, or none if the vertex isn't present in the
   *         embedding.
   */
  override def embeddingFor(vertex: VType): AdjacencyList[VType] = {
    val vertexIndex = vertexIndices.get(vertex).getOrElse(throw new NoSuchElementException("The vertex " + vertex + " was not embedded.") )
    embeddingFor(vertexIndex)
  }

  /**
   * @return every vertex in the embedding.
   */
  override def embeddedVertices: Vector[VType] = adjacencyLists.map(_._1)

  /**
   * @return Every embedded edge in O(n) time.
   */
  override def edges: Vector[(VType, VType)] = {
    val addedEdges = new mutable.HashSet[UnorderedPair[VType]]()
    val result = new ListBuffer[(VType, VType)]()
    var vertexIndex = 0

    while (vertexIndex < adjacencyLists.size) {
      val vertex = adjacencyLists(vertexIndex)._1
      val adjacencyList = adjacencyLists(vertexIndex)._2
      var neighborIndex = 0

      while (neighborIndex < adjacencyList.size) {
        val neighborVertexIndex = adjacencyList(neighborIndex)
        val neighborVertex = adjacencyLists(neighborVertexIndex)._1
        val edge = UnorderedPair(vertex, neighborVertex)

        if (!addedEdges.contains(edge)) {
          result += edge.asTuple
          addedEdges += edge
        }

        neighborIndex += 1
      }

      vertexIndex += 1
    }

    result.toVector
  }

  /**
   * @return every vertex and its head entry in the embedding in O(n) time.
   */
  override def iterator: Iterator[(VType, AdjacencyList[VType])] = {
    (for (i <- 0 until adjacencyLists.size) yield {
      val index = if (adjacencyLists(i)._2.isEmpty) -1 else 0
      (adjacencyLists(i)._1, new VertexEntry(i, index, this))
    }).toIterator
  }

  /**
   * Embeds a new edge.
   * @param head A vertex to be embedded.
   * @param tail A vertex to be embedded, belonging to the same edge as the Head.
   * @return A new embedding with the edge added to it, or the same embedding if both vertices already exist and
   *         contain an edge between each other.
   */
  override def embed(head: VType, tail: VType): Embedding[VType] = embed(head, tail, None, None)

  /**
   * Embeds a new edge. Regardless of weather the edge is undirected or directed,
   * it contains a head and a tail node (these are arbitrary on undirected edges).
   * Vertices will be given an empty adjacency list if they're not already present,
   * and each will have the opposite node added to its embedding.
   *
   * Example usage: embed(A, B, A_C, B_F)
   *
   * The above call embeds an edge between A and B. Both A and B exists in the embeddings, but does not have
   * an edge inserted between them. The entry of B will be inserted in A's adjacency list after the edge entry
   * pointing to the vertex C. The entry of A in B will be inserted after the entry pointing to the vertex F.
   *
   * Example usage: embed(A, B, A_C, None)
   *
   * This call can be made to embed the edge (A, B) if B doesn't already exist in the embedding (the entry to A will be
   * the only entry in B's adjacency list, and thus does not need a specified position), or if the entry of A in B may
   * be inserted at an arbitrary position.
   *
   * @param head A vertex to be embedded.
   * @param tail A vertex to be embedded, belonging to the same edge as the Head.
   * @param headPos Node that the Head should be placed after in the Tails embedding. None if it should be
   *                added to the end of the adjacency list or if this is the first edge to embedded into the tail.
   * @param tailPos Node that the Tail should be placed after in the Heads embedding. None if it should be
   *                added to the end of the adjacency list or if this is the first edge to embedded into the head.
   * @return A new embedding with the edge added to it.
   */
  override def embed(head: VType, tail: VType, headPos: Option[VType], tailPos: Option[VType]): Embedding[VType] = {
    if (embeddingExists(head, tail)) return this
    if (head == tail) throw new IllegalArgumentException("Attempted to embed an edge beginning and ending in the same vertex (" + head + ")")

    if (headPos.isDefined && (embeddingFor(tail).isEmpty || !embeddingFor(tail).containsEntryFor(headPos.get)))
      throw new IllegalArgumentException("Attempted to specify insert position for " + head + " as " + headPos.get + " in the vertex " + tail + ", but that position doesn't exist.")

    if (tailPos.isDefined && (embeddingFor(head).isEmpty || !embeddingFor(head).containsEntryFor(tailPos.get)))
      throw new IllegalArgumentException("Attempted to specify insert position for " + tail + " as " + tailPos.get + " in the vertex " + head + ", but that position doesn't exist.")

    if (!vertexIndices.contains(head)) {
      embed(head).embed(head, tail, headPos, tailPos)
    } else if (!vertexIndices.contains(tail)) {
      embed(tail).embed(head, tail, headPos, tailPos)
    } else {
      val headIndex = vertexIndices(head)
      val tailIndex = vertexIndices(tail)
      val headAdjacencies = adjacencyLists(headIndex)._2
      val tailAdjacencies = adjacencyLists(tailIndex)._2

      val headPositionInTail = if (headPos.isDefined) {
        val pIndex = vertexIndices(headPos.get)
        positionOfNeighborInVertex(tailIndex, pIndex) + 1 // Insert should occurs after
      } else {
        tailAdjacencies.size
      }

      val tailPosInHead = if (tailPos.isDefined) {
        val pIndex = vertexIndices(tailPos.get)
        positionOfNeighborInVertex(headIndex, pIndex) + 1
      } else {
        headAdjacencies.size
      }

      val updatedHeadAdjacency = addVerticesToAdjacency(Vector(tailIndex), headAdjacencies, tailPosInHead)
      val updatedTailAdjacency = addVerticesToAdjacency(Vector(headIndex), tailAdjacencies, headPositionInTail)

      val adjacencyVectorWithHead = adjacencyLists.updated(headIndex, (head, updatedHeadAdjacency))
      val adjacencyVectorWithTail = adjacencyVectorWithHead.updated(tailIndex, (tail, updatedTailAdjacency))

      var updatedAdjacencyPositions = adjustNeighborCoordinates(headIndex, tailPosInHead, 1, adjacencyPositions)
      updatedAdjacencyPositions = adjustNeighborCoordinates(tailIndex, headPositionInTail, 1, updatedAdjacencyPositions)

      updatedAdjacencyPositions = addNeighborPosition(headIndex, tailIndex, tailPosInHead, updatedAdjacencyPositions)
      updatedAdjacencyPositions = addNeighborPosition(tailIndex, headIndex, headPositionInTail, updatedAdjacencyPositions)

      new UndirectedEmbedding(vertexIndices, adjacencyVectorWithTail, updatedAdjacencyPositions, size)
    }
  }

  /**
   * Embeds an edge A~B between two vertices A and B, specifying which vertex in B's adjacency list that A should be
   * inserted after, and vice versa.
   * @param insertData DSL specifying insertion points for each vertex, or default insertion.
   * @return This mutable embedding, allows chained embed calls.
   */
  override def embedEdge(insertData: EdgeInsert[VType]): Embedding[VType] = {
    embed(insertData.firstVertexToInsert, insertData.secondVertexToInsert, insertData.positionToInsertInSecondVertex, insertData.positionToInsertInFirstVertex)
  }

  /**
   * Embeds a single vertex with an empty adjacency list.
   * @param vertex Vertex to embed.
   * @return This mutable embedding, allows chained embed calls.
   */
  override def embed(vertex: VType): Embedding[VType] = {
    if (vertexIndices.contains(vertex)) {
      this

    } else {
      val nextIndex = size
      val newIndexMap = vertexIndices + (vertex -> nextIndex)
      val newAdjacencies = adjacencyLists :+ (vertex, Vector())
      val emptyAdjacencyPositions = adjacencyPositions + (nextIndex -> Map[Int, Int]())
      new UndirectedEmbedding(newIndexMap, newAdjacencies, emptyAdjacencyPositions, size + 1)
    }
  }

  /**
   * Constructs a complete embedding using adjacencies around each vertex.
   * @param adjacencies A map between every vertex in the embedding and the vertices adjacent to it in the order
   *                    they should be embedded. Note that no vertex in this map may already be present in the
   *                    embedding. If so is the case, embed them separately then join them at specified insertion
   *                    points.
   * @return A copy of this embedding with every specified vertex mapped to its adjacency list.
   */
  override def embed(adjacencies: Map[VType, Vector[VType]]): Embedding[VType] = {
    var currentIds = vertexIndices
    var currentAdjacencies = expandAdjacencyList(adjacencyLists, adjacencies.size)
    var currentPositions = adjacencyPositions
    var nextId = size

    for (newVertex <- adjacencies.keys) {
      currentIds = currentIds + (newVertex -> nextId)
      nextId += 1
    }

    for (entry <- adjacencies) {
      val vertexIndex = currentIds(entry._1)
      val adjacencyList = entry._2.map(currentIds(_))
      val positions = (for (i <- 0 until adjacencyList.size) yield adjacencyList(i) -> i).toMap
      currentAdjacencies = currentAdjacencies.updated(vertexIndex, (entry._1, adjacencyList))
      currentPositions = currentPositions + (vertexIndex -> positions)
    }

    new UndirectedEmbedding(currentIds, currentAdjacencies, currentPositions, adjacencies.size)
  }

  /**
   * Removes an edge from the embedding.
   * @param head A vertex on the edge.
   * @param tail Another vertex on the edge.
   * @return This mutable embedding with the edge removed, allows chained embed calls.
   */
  override def deleteEdge(head: VType, tail: VType): Embedding[VType] = {
    val headIndex = vertexIndices(head)
    val tailIndex = vertexIndices(tail)
    edgeDeletion(headIndex, tailIndex)
  }

  /**
   * Removes a vertex and any edges connected to it from the embedding.
   * @param vertex Vertex to remove.
   * @return This mutable embedding, allows chained embed calls.
   */
  override def deleteVertex(vertex: VType): Embedding[VType] = {
    val index = vertexIndices(vertex)
    val adjacencies = adjacencyLists(index)._2

    if (!adjacencies.isEmpty) {
      edgeDeletion(index, adjacencies.head).deleteVertex(vertex)

    } else {

      /* Deleting a vertex would leave a gap in the adjacency array unless every other id is shifted down by 1. */
      val updatedAdjacencyList = adjacencyLists.patch(from = index, patch = Nil, replaced = 1)
        .map(kv => kv._1 -> kv._2.map(id => if (id > index) id - 1 else id))
      val updatedIndices = vertexIndices.filter(_._2 != index).map(kv => (kv._1, if (kv._2 > index) kv._2 - 1 else kv._2))
      val updatedPositions = (adjacencyPositions - index)
        .map(idAndMap => {
        val newId = if (idAndMap._1 > index) idAndMap._1 - 1 else idAndMap._1
        val newMap = idAndMap._2.map(idAndPos => {
          val newId = if (idAndPos._1 > index) idAndPos._1 - 1 else idAndPos._1
          (newId, idAndPos._2)
        })
        (newId, newMap)
      })

      new UndirectedEmbedding(updatedIndices, updatedAdjacencyList, updatedPositions, size - 1)
    }
  }

  /**
   * Creates a new embedding by joining two embeddings together in a single vertex, at a position
   * in this embedding specified by the user.
   *
   * @param other Embedding to combine with this one.
   * @param positions Edges in this embedding, mapped towards the vertex of the entry that any edges from 'other
   *                  should be inserted after. This map needs to have a key for every vertex common to both embeddings,
   *                  or an IllegalArgumentException will be thrown.
   * @return A new embedding with all edges from both previous embeddings. The shares vertex will contain edges
   *         from both embeddings, and all edges from a single embedding will retain the circular order they had
   *         before the join.
   */
  override def addEmbedding(other: Embedding[VType], positions: Map[VType, VType]): Embedding[VType] = {
    if (!other.isInstanceOf[UndirectedEmbedding[VType]])
      throw new Error("Attempted to join an embedding, but the underlying class was not " + this.getClass)

    val otherEmbedding = other.asInstanceOf[UndirectedEmbedding[VType]]
    val commonVertices = embeddedVertices.toSet intersect otherEmbedding.embeddedVertices.toSet

    /* Every connecting vertex must be specified in the position map. */
    if (commonVertices != positions.keySet) {
      throw new IllegalArgumentException("The positions " + positions + " does not equal the set of common vertices in both embeddings: " + commonVertices)
    }

    /* Every vertex not in this embedding needs to be assigned its own id. */
    var nextAvailableId = size
    val updatedIds = new mutable.HashMap[VType, Int]()
    for (v <- otherEmbedding.embeddedVertices) {
      if(!commonVertices.contains(v)) {
        updatedIds += v -> nextAvailableId
        nextAvailableId += 1
      } else {
        updatedIds += v -> vertexIndices(v)
      }
    }

    /* Every old id of a new vertex must be converted to the newly assigned id before adding the new adjacency
     * lists to this embedding. The only ids that doesn't need changing are the ones belonging to a common
     * vertex.
     */
    val oldToNewIds: Map[Int, Int] = otherEmbedding.vertexIndices.map(kv => kv._2 -> updatedIds(kv._1))
    val updatedAdjacencyListIds: Vector[(VType, Vector[Int])] = otherEmbedding.adjacencyLists.map(vAndAdj => (vAndAdj._1, vAndAdj._2.map(oldId => {
      if (oldToNewIds.contains(oldId))
        oldToNewIds(oldId)
      else
        vertexIndices(vAndAdj._1)
    })))

    /* Since some of the vertices might exist in both embeddings, a new vector needs to be constructed with the new
     * entries on the index matching their id.
     */
    var finalAdjacencyVector = expandAdjacencyList(adjacencyLists, nextAvailableId - size)

    var i = 0
    while (i < updatedAdjacencyListIds.size) {
      val vertex = updatedAdjacencyListIds(i)._1

      if (!commonVertices.contains(vertex)) {
        val newIndex = updatedIds(vertex)
        finalAdjacencyVector = finalAdjacencyVector.updated(newIndex, updatedAdjacencyListIds(i))
      }

      i += 1
    }

    /* Common vertices needs to have their adjacency lists updated by inserting the lists of the other embedding
     * at the specified insertion point given by the positional map.
     */
    var updatedPositions = adjacencyPositions

    for (c <- commonVertices) {
      val oldIndex = otherEmbedding.vertexIndices(c)
      val currentIndex = vertexIndices(c)
      val newAdjacencies = updatedAdjacencyListIds(oldIndex)._2
      var currentAdjacencies = finalAdjacencyVector(currentIndex)._2
      val idOfInsertPos = vertexIndices(positions(c))
      val positionOfInsertVertexInCommonVertex: Int = adjacencyPositions(currentIndex)(idOfInsertPos) + 1

      currentAdjacencies = addVerticesToAdjacency(newAdjacencies, currentAdjacencies, positionOfInsertVertexInCommonVertex)
      finalAdjacencyVector = finalAdjacencyVector.updated(currentIndex, (c, currentAdjacencies))
      updatedPositions = adjustNeighborCoordinates(currentIndex, positionOfInsertVertexInCommonVertex, newAdjacencies.size, updatedPositions)

      for (i <- 0 until newAdjacencies.size) {
        val insertPos = positionOfInsertVertexInCommonVertex + i
        val neighborIndex = currentAdjacencies(insertPos)
        updatedPositions = addNeighborPosition(currentIndex, neighborIndex, insertPos, updatedPositions)
      }
    }

    /* Every adjacency entry in the other embedding needs to have its indices converted to the ones used in this
     * embedding.
     */
    val otherCommonIndices: Set[Int] = otherEmbedding.vertexIndices.filter(kv => commonVertices.contains(kv._1)).values.toSet
    val updatedNewPositions = otherEmbedding.adjacencyPositions
      .filter(kv => !otherCommonIndices.contains(kv._1))
      .map(kv => oldToNewIds(kv._1) -> kv._2.map(kv => oldToNewIds(kv._1) -> kv._2))

    val finalIdMap = vertexIndices ++ updatedIds.toMap
    new UndirectedEmbedding(finalIdMap, finalAdjacencyVector, updatedPositions ++ updatedNewPositions, finalIdMap.size)
  }

  /**
   * Reverses every adjacency entrys next/previous links in the embedding.
   * @return A new embedding with every adjacency list reversed.
   */
  override def reverse: Embedding[VType] = {
    var currentAdjacencies = adjacencyLists
    var currentPositions = adjacencyPositions

    for (i <- vertexIndices.values) {
      val reversed = reverseAdjacencyAndPositions(i, currentAdjacencies, currentPositions)
      currentAdjacencies = reversed._1
      currentPositions = reversed._2
    }

    new UndirectedEmbedding(vertexIndices, currentAdjacencies, currentPositions, size)
  }

  /**
   *
   * @param f Function that takes a vertex in the embedding and outputs another vertex.
   * @tparam N Output type.
   * @return A copy of this embedding with every vertex mapped to the ones produced by the function.
   */
  override def map[N](f: VType => N): Embedding[N] = {
    val newVertices = vertexIndices.keys.map(oldVertex => oldVertex -> f(oldVertex)).toMap
    val newIndexMap = vertexIndices.map(kv => newVertices(kv._1) -> kv._2)
    val newAdjacencies = adjacencyLists.map(l => (newVertices(l._1), l._2))
    new UndirectedEmbedding[N](newIndexMap, newAdjacencies, adjacencyPositions, size)
  }

  /**
   * @param v Vertex to retrieve neighbors for.
   * @return Every neighboring vertex of v.
   */
  override def neighborsOf(v: VType): Vector[VType] = embeddingFor(v).toVector.map(_.adjacentVertex)

  /** @return The number of embedded vertices. */
  override def vertexSize: Int = embeddedVertices.size //TODO Make it constant if possible.

  override def toString: String = {
    val buffer = new StringBuilder()
    val it = this.iterator

    buffer ++= "Undirected embedding\n"

    while (it.hasNext) {
      val entry: (VType, AdjacencyList[VType]) = it.next()
      buffer ++= entry._1.toString + " -> " + entry._2.toString + "\n"
    }

    buffer.mkString
  }

  override def equals(other: Any): Boolean = other match {
    case u: UndirectedEmbedding[VType] => this.## == u.## && u.vertexIndices == vertexIndices && u.adjacencyLists == adjacencyLists && u.adjacencyPositions == adjacencyPositions
    case _ => false
  }

  override val hashCode: Int = vertexIndices.## ^ adjacencyLists.## ^ adjacencyPositions.##

  private def expandAdjacencyList(list: Vector[(VType, Vector[Int])], amount: Int): Vector[(VType, Vector[Int])] = {
    list ++ Vector.fill(amount)(null)
  }

  private def edgeDeletion(vertex: Int, neighbor: Int): UndirectedEmbedding[VType] = {
    var updatedData = removeAdjacency(vertex, neighbor, adjacencyLists, adjacencyPositions)
    updatedData = removeAdjacency(neighbor, vertex, updatedData._1, updatedData._2)
    new UndirectedEmbedding(vertexIndices, updatedData._1, updatedData._2, size)
  }

  private def reverseAdjacencyAndPositions(vertex: Int,
                                           adjacencyList: Vector[(VType, Vector[Int])],
                                           positions: Map[Int, Map[Int, Int]]): (Vector[(VType, Vector[Int])], Map[Int, Map[Int, Int]]) = {
    val adjacencies = adjacencyList(vertex)._2.reverse
    val reversedPositionMap: IndexedSeq[(Int, Int)] = for (i <- 0 until adjacencies.size) yield adjacencies(i) -> i

    (adjacencyList.updated(vertex, (adjacencyList(vertex)._1, adjacencies)), positions + (vertex -> reversedPositionMap.toMap))
  }

  /** Removes an adjacency from a vertex adjacency list and updates all positional indices after it. */
  private def removeAdjacency(vertex: Int, neighbor: Int,
                              adjacencyList: Vector[(VType, Vector[Int])],
                              positions: Map[Int, Map[Int, Int]]): (Vector[(VType, Vector[Int])], Map[Int, Map[Int, Int]]) = {
    val v = adjacencyList(vertex)._1
    val vertexAdjacencies = adjacencyList(vertex)._2
    val positionOfNeighbor = positions(vertex)(neighbor)
    val adjacenciesWithoutNeighbor = vertexAdjacencies patch (from = positionOfNeighbor, patch = Nil, replaced = 1)

    val positionsForVertex = positions(vertex) - neighbor
    val updatedPositions = adjustNeighborCoordinates(vertex, positionOfNeighbor, -1, positions + (vertex -> positionsForVertex))

    (adjacencyList.updated(vertex, v -> adjacenciesWithoutNeighbor), updatedPositions)
  }

  /**
   * @param vs Vertices to add into an adjacency list.
   * @param adj Current adjacency list.
   * @param position Position in adjacency list to add vertices to.
   * @return The resulting adjacency list.
   */
  private def addVerticesToAdjacency(vs: Vector[Int], adj: Vector[Int], position: Int): Vector[Int] = {
    val split = adj.splitAt(position)
    split._1 ++ vs ++ split._2
  }

  private def adjustNeighborCoordinates(vertex: Int, startCoordinate: Int, amount: Int, currentPositions: Map[Int, Map[Int, Int]]): Map[Int, Map[Int, Int]] = {
    val neighborData = currentPositions(vertex)
    val adjustedNeighborData = neighborData.map(kv => {
      if (kv._2 >= startCoordinate)
        kv._1 -> (kv._2 + amount)
      else
        kv
    })

    currentPositions + (vertex -> adjustedNeighborData)
  }

  private def addNeighborPosition(vertex: Int, neighbor: Int, position: Int, currentPositions: Map[Int, Map[Int, Int]]): Map[Int, Map[Int, Int]] = {
    val pos = currentPositions(vertex)
    currentPositions + (vertex -> (pos + (neighbor -> position)))
  }

  private def embeddingFor(index: Int): AdjacencyList[VType] = {
    val adjacencies = adjacencyLists(index)._2
    val startIndex = if (adjacencies.isEmpty) -1 else 0
    new VertexEntry(index, startIndex, this)
  }

  private def positionOfNeighborInVertex(vertex: Int, neighbor: Int): Int = adjacencyPositions(vertex)(neighbor)

  private def embeddingExists(a: VType, b: VType): Boolean = {
    val aIndex = vertexIndices.get(a).getOrElse(return false)
    val bIndex = vertexIndices.get(b).getOrElse(return false)
    val aEntry = embeddingFor(aIndex)
    val bEntry = embeddingFor(bIndex)

    aEntry.containsEntryFor(b) && bEntry.containsEntryFor(a)
  }

}
