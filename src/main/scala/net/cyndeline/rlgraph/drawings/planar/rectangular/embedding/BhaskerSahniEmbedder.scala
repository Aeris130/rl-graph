package net.cyndeline.rlgraph.drawings.planar.rectangular.embedding

import net.cyndeline.rlgraph.biconnectivity.components.DFSComponentSearch
import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.drawings.planar.rectangular.embedding.help._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Embeds a properly triangulated graph in linear time according to the algorithm presented by Jayaram Bhasker and
 * Sartaj Sahni in the paper "A linear time algorithm to check for the existence of a rectangular dual of a planar
 * triangulated graph".
 *
 * This embedder is also used to check for the existence of a rectangular drawing, since failing to yield an embedding
 * means that the graph lacks such a drawing.
 *
 * An embedding is guaranteed to exist if:
 *  - Every interior face is a triangle
 *  - All internal vertices has degree >= 4
 *  - All cycles that are not faces have length >= 4
 */

class BhaskerSahniEmbedder[VType : TypeTag : ClassTag] {
  private val componentAndCutpointFinder = new DFSComponentSearch[VType, UnDiEdge]()
  private val componentValidator = new ComponentValidator()
  private val vertexFinder = new CommonVertexFinder()
  private val triangleEmbedder = new TriangleEmbedder()
  private val merger = new OuterEmbeddingMerger[VType](faces => Some(faces.find(face => face.vertexSize > 3).getOrElse(faces.head)))

  /**
   * Embeds a properly triangulated graph.
 *
   * @param graph A planar graph consisting of biconnected components.
   * @return A planar embedding where the face having length > 3 is the outer face (if one exists), or None if the
   *         graph isn't properly triangular.
   */
  def embed(graph: Graph[VType, UnDiEdge]): Option[Embedding[VType]] = {
    if (graph.isEmpty || !graph.isConnected) return None

    /* Step 1: Compute all biconnected components and their articulation points. */
    val componentsAndCutpoints = componentAndCutpointFinder.componentsAndArticulationPoints(graph)

    /* Step 2: If any component fails validation, the total graph has no valid embedding. */

    // A list of every biconnected component, the number of common vertices in it, and its cutpoints
    val componentsAndCommonVertices: Vector[(Graph[VType, UnDiEdge], Int, Set[VType])] =
      for {
        compAndCut: (Graph[VType, UnDiEdge], Set[VType]) <- componentsAndCutpoints
        commonVertices = componentValidator.hasEmbedding[VType, UnDiEdge](compAndCut._1).getOrElse {
          return None // No embedding possible
        }
      } yield (compAndCut._1, commonVertices, compAndCut._2)

    /* Step 3: Embed each component separately. */
    var embeddedData = Vector[(Embedding[VType], Set[VType])]() // Which cutpoints exist in an embedding
    var embeddingsAtCutpoint = Map[VType, Set[Embedding[VType]]]() // Which embeddings contains a cutpoint
    val componentsToEmbed = componentsAndCommonVertices.toIterator
    while (componentsToEmbed.hasNext) {
      val component = componentsToEmbed.next()
      val graph = component._1
      val commonVertexAmount = component._2
      val cutpointsInComponent = component._3
      val embedding = embedComponent(graph, cutpointsInComponent, commonVertexAmount)

      if (embedding.isEmpty) {
        return None // If a single component fails the embedding, the total graph cannot be embedded
      } else {
        for (c <- cutpointsInComponent) {
          val currentEmbeddingsAroundCutpoint = embeddingsAtCutpoint.get(c).getOrElse(Set())
          embeddingsAtCutpoint += (c -> (currentEmbeddingsAroundCutpoint + embedding.get))
        }
        embeddedData = (embedding.get, cutpointsInComponent) +: embeddedData
      }
    }

    /* Step 4: Merge embeddings at their articulation points. */
    val embeddingsToCutpoints: Map[Embedding[VType], Set[VType]] = embeddedData.toMap
    val cutpointsToEmbedding = embeddingsAtCutpoint
    Some(merger.mergeEmbeddings(embeddedData.map(e => e._1), cutpointsToEmbedding, embeddingsToCutpoints))
  }

  /**
   * Embeds a single biconnected component.
 *
   * @param graph Sub-graph representing the component.
   * @param articulationPoints Every articulation point on the component
   * @param commonVertices Number of common vertices in the component
   * @return A mutable embedding of the component, or None if it (and by extension, the total graph) cannot be embedded
   */
  private def embedComponent(graph: Graph[VType, UnDiEdge], articulationPoints: Set[VType], commonVertices: Int): Option[Embedding[VType]] = {
    var newVertices: Set[VType] = graph.nodes.map(n => { val v: VType = n; v }).toSet
    var nonCoveredEdges: Set[UnDiEdge[VType]] = graph.edges.map(e => e.toOuter).toSet
    var trianglesDrawn = 0
    var startVertices = Set[VType]() // Every vertex that has already had the start position on the outer face

    var embedding: Embedding[VType] = UndirectedEmbedding[VType]()

    /* Step 1 (setup): Pick an arbitrary edge and any one of its common vertices and embed an initial triangle. */
    val edge = graph.edges.head.toOuter
    val commonVertex = vertexFinder.findCommonVertices(edge, graph).head
    embedding = embedding.embed(edge._1, edge._2)
    embedding = triangleEmbedder.embedNewTriangle(commonVertex, edge._1, edge._2, embedding)

    /* Initiate the outer boundary of the embedding as the vertices in the initial triangle, in clockwise order.
     * (Ergo, moving to the next entry on the list moves the pointer clockwise on the face, while previous is
     * counterclockwise).
     *
     * Always points at the current start vertex.
     */
    val boundary = new OuterBoundary[VType](commonVertex, embedding)

    /* Set the three vertices as old, the three edges as covered, and note that 1 triangle has been embedded. */
    newVertices --= Set(commonVertex, edge._1, edge._2)
    val commonEdges = graph.edges
      .filter(e => e.contains(commonVertex) && (e.contains(edge._1) || e.contains(edge._2)))
      .map(e => e.toOuter)
      .toSet
    nonCoveredEdges --= (commonEdges + edge)

    trianglesDrawn = 1

    /* Set the start vertex to an arbitrary vertex on the outer face. */
    var start = commonVertex
    boundary.markAsCurrent(commonVertex)
    startVertices += start

    /* Step 2: Repeat step 3 and 4 until the component is embedded, or no embedding is found possible. */
    var finished = false

    while (!finished) {

      /* Step 3: Next vertex is the vertex counter clockwise to the start vertex on the outer face. */
      val nextVertex = boundary.counterClockwiseVertex

      /* Find a vertex common to start and next that is connected by an edge that isn't covered. */
      val common: Option[VType] = findCommonUncoveredVertex(edgeBetween(start, nextVertex, graph), graph, nonCoveredEdges)

      /* Step 4 */
      if (common.isEmpty) {

        /* No common vertices found. Advance the start vertex counter clockwise on the outer face. If the next
         * vertex has already been start once, we're done.
         */
        if (startVertices.contains(nextVertex)) {
          finished = true
        } else {
          start = nextVertex
          boundary.markAsCurrent(start)
          startVertices += nextVertex
        }

      } else if (common.isDefined && newVertices.contains(common.get)) {
        val newVertex = common.get

        /* Embed the new vertex. Note that Next is submitted before Start, since they appear in the order
         * Next -> Start when moving clockwise on the face.
         */
        embedding = triangleEmbedder.embedNewTriangle(newVertex, nextVertex, start, embedding)

        /* Add the new vertex between start and next. */
        boundary.add(newVertex, boundary.counterClockwiseVertex)

        /* Make the new vertex old, mark the edges as covered and add a new triangle as drawn. */
        newVertices -= newVertex
        nonCoveredEdges -= edgeBetween(start, newVertex, graph)
        nonCoveredEdges -= edgeBetween(nextVertex, newVertex, graph)
        trianglesDrawn += 1

      } else { /* The vertex is old. */

        // Three sub-cases
        val oldVertex = common.get
        val ccOfNextVertex = boundary.counterClockwiseVertex(2)
        val clockwiseOfStart = boundary.clockwiseVertex

        if (ccOfNextVertex == oldVertex) {

          /* The old vertex lies counterclockwise of nextVertex on the outer face */

          if (articulationPoints.contains(nextVertex) || hasUncoveredEdges(nextVertex, nonCoveredEdges, graph)) {
            /* No embedding possible */
            return None

          } else {

            /* Since the old vertex lies cc of nextVertex, the edge to nextVertex already exists in the embedding.
             * The insert position of start in the old vertex is before nextVertex. The insert position of the old
             * vertex in start is clockwise of nextVertex.
             */
            val oldInsert = embedding.embeddingFor(oldVertex).entryFor(nextVertex).previous.adjacentVertex
            val startInsert = nextVertex
            embedding = triangleEmbedder.embedOldTriangleEdge(oldVertex, startInsert, start, oldInsert, embedding)
            boundary.remove(nextVertex)
            boundary.markAsCurrent(start)
            nonCoveredEdges -= edgeBetween(oldVertex, start, graph)
            trianglesDrawn += 1
          }

        } else if (clockwiseOfStart == oldVertex) {

          /* The old vertex is clockwise from the start vertex on the outer boundary. */

          if (articulationPoints.contains(start) || hasUncoveredEdges(start, nonCoveredEdges, graph)) {
            /* No embedding possible */
            return None

          } else {

            /* Since the old vertex lies clockwise to start, the edge between start and old already exists.
             * Insert a single edge between nextVertex and the old vertex. The insert position for the old
             * vertex in NextVertex is cc of start. The insert position for nextVertex in the old vertex is
             * clockwise of start.
             */
            val oldInsert = start
            val nextInsert = embedding.embeddingFor(nextVertex).entryFor(start).previous.adjacentVertex
            embedding = triangleEmbedder.embedOldTriangleEdge(oldVertex, nextInsert, nextVertex, oldInsert, embedding)
            boundary.remove(start)
            boundary.markAsCurrent(oldVertex)
            nonCoveredEdges -= edgeBetween(oldVertex, nextVertex, graph) // mark oldVertex~nextVertex as covered
            trianglesDrawn += 1
            start = oldVertex
            startVertices += oldVertex
          }

        } else { // Third sub-case, the old vertex is neither clockwise from Start, nor counter clockwise from Next

          /* Move forward. The triangle(s) involving the old vertex might be drawn later. */
          if (startVertices.contains(nextVertex)) {
            finished = true
          } else {
            start = nextVertex
            boundary.markAsCurrent(start)
            startVertices += nextVertex
          }
        } // End of third sub-case
      } // End of old-vertex case
    } // End of while-!finished loop

    Some(embedding)
  }

  /* Finds a vertex common to an edge, given that at least one of its edges aren't covered yet. */
  private def findCommonUncoveredVertex(edge: UnDiEdge[VType], graph: Graph[VType, UnDiEdge], nonCovered: Set[UnDiEdge[VType]]): Option[VType] = {
    val commonVertices = vertexFinder.findCommonVertices(edge, graph).toSet
    commonVertices.find(v => graph.edges.exists(e => nonCovered.contains(e.toOuter) && e.contains(v) && (e.contains(edge._1) || e.contains(edge._2))))
  }

  private def hasUncoveredEdges(vertex: VType, nonCoveredEdges: Set[UnDiEdge[VType]], graph: Graph[VType, UnDiEdge]): Boolean = {
    graph.get(vertex).edges.exists(e => nonCoveredEdges.contains(e.toOuter))
  }

  /* Retrieves the outer edge between two vertices in a graph. */
  private def edgeBetween(a: VType, b: VType, graph: Graph[VType, UnDiEdge]): UnDiEdge[VType] = graph.edges.find(e => e.contains(a) && e.contains(b)).get.toOuter
}
