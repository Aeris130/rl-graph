package net.cyndeline.rlgraph.regularEdgeLabeling.angularMap

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.embedding.{Embedding, Vertex}
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.regularEdgeLabeling.EdgeLabeling

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
 * An angular map is a data structure based on the edges of both the set T1 and T2 of a regular edge labeling, as well
 * as the edges connecting the outer vertices North, South, West, East. It is a graph with black and white vertices:
 * Black vertices represents the original vertices in the REL, while the white vertices represents the faces of the
 * PTP graph that the REL was based upon.
 *
 * The edges of the graph are also oriented with a direction. Every black vertex B has an outgoing edge to all white
 * vertices that belongs to faces adjacent to B in the PTP graph. Every white vertex W has two incoming and one outgoing
 * edge from the black vertices associated to its triangular face in the PTP graph: If the black vertex B lies between
 * two edges of the same set (T1/T2) in the REL, orient the edge towards B. Otherwise orient it towards W.
 *
 * One exception to this is the white vertex representing the outer face. As the edges bounding the outer face are not
 * assigned to a set in the REL, it will have out-degree 0 and represent a meaningless angle in any drawing constructed
 * by the REL. For this reason it is excluded from the map.
 *
 * The actual edges of the PTP graph are not included in the map. This results in a graph where every face is
 * quadrangular, and every black vertex has out-degree 4. The angular map combined with the edge-orientations associated
 * with it is also referred to as an alpha4-orientation, and can be used to compute the combinatorial set of REL's
 * that can be derived from T1 and T2.
 *
 * Recommended reading: "Finding a square dual of a graph", T.Picchetti
 */
class AngularMap[V : TypeTag : ClassTag](rel: EdgeLabeling[V]) {

  /* Contains both edges between B/W vertices and the original REL edges. Needed when computing orientations. */
  private val totalEmbeddingAndMap = constructEmbedding
  private val totalEmbedding = totalEmbeddingAndMap._1

  /** White and black vertices embedded using undirected edges. */
  val angularEmbedding: Embedding[AngularVertex[V]] = computeAngularEmbedding(totalEmbedding)

  /** Maps the faces in the embedding that combines the sets T1 and T2 in the original REL to the vertex that
    * represents them in the angular map.
    */
  val faceVertex: Map[Face[V], AngularVertex[V]] = totalEmbeddingAndMap._2

  /* For each pair representing an edge in the angular graph, this map returns the vertex the edge is oriented at. */
  private val orientations: Map[UnorderedPair[AngularVertex[V]], AngularVertex[V]] = constructOrientations

  /**
   * @param v1 A vertex of an edge in this angular map.
   * @param v2 A vertex opposite of v1.
   * @return The vertex of v1 and v2 that the edge between them is oriented towards.
   */
  def orientationOfEdge(v1: AngularVertex[V], v2: AngularVertex[V]): AngularVertex[V] = {
    orientations.getOrElse(UnorderedPair(v1, v2), throw new Error("The edge " + v1 + " to " + v2 + " is not a part of the angular map."))
  }

  /* The easiest way is to map the original embedding to black vertices, embed white vertices inside the face then
   * delete all the original edges.
   */
  private def constructEmbedding: (Embedding[AngularVertex[V]], Map[Face[V], AngularVertex[V]]) = {
    val vertices: Map[V, AngularVertex[V]] = rel.originalEmbedding
      .embeddedVertices
      .zipWithIndex
      .map(vi => vi._1 -> new AngularVertex(vi._1, vi._2))
      .toMap
    var embedding = rel.originalEmbedding.map((v: V) => vertices(v))
    val faces = new FaceComputation[AngularVertex[V]]().computeFaces(embedding)
    var faceToWhiteMap = Map[Face[V], AngularVertex[V]]()

    var nextVertexId: Int = vertices.map(v => v._2.id).max + 1
    for (f <- faces if f.vertices.filter(_.isBlack).map(_.vertex).toSet != Set(rel.north, rel.west, rel.south, rel.east)) {
      val newWhiteVertex = new AngularVertex[V](nextVertexId)
      faceToWhiteMap += (new Face(f.vertices.map(_.vertex)) -> newWhiteVertex)
      nextVertexId += 1
      val edgePairs: Vector[((AngularVertex[V], AngularVertex[V]), (AngularVertex[V], AngularVertex[V]))] = f.edges zip (f.edges.drop(1) :+ f.edges.head)

      /* Embed an edge from the common vertex of both consecutive edges to the white vertex. */
      for (p <- edgePairs) {
        val common = p._1._2
        val insertPointInCommon = p._2._2
        embedding = embedding.embedEdge(Vertex(common) withDefaultPositionInVertex newWhiteVertex withInsertPosition insertPointInCommon)
      }
    }

    (embedding, faceToWhiteMap)
  }

  /* Removes the REL edges from the total embedding. */
  private def computeAngularEmbedding(e: Embedding[AngularVertex[V]]): Embedding[AngularVertex[V]] = {
    var embedding = e
    for (edge <- embedding.edges if edge._1.isBlack && edge._2.isBlack) {
      embedding = embedding.deleteEdge(edge._1, edge._2)
    }

    embedding
  }

  /* Decide if each edge should point to its black or its white vertex. */
  private def constructOrientations = {
    val newToOldVertices = angularEmbedding.embeddedVertices.filter(_.isBlack).map(v => v.vertex -> v).toMap
    var oriented = Map[UnorderedPair[AngularVertex[V]], AngularVertex[V]]()
    val whiteVertices = angularEmbedding.embeddedVertices.filter(_.isWhite)
    val t1Edges = rel.edgesOfT1.map(e => UnorderedPair(newToOldVertices(e.from), newToOldVertices(e.to))).toSet
    val t2Edges = rel.edgesOfT2.map(e => UnorderedPair(newToOldVertices(e.from), newToOldVertices(e.to))).toSet
    var outerEdgeCount = 0

    for (v <- whiteVertices; neighbor <- totalEmbedding.embeddingFor(v).iterator.map(_.adjacentVertex)) {
      val whiteEntryInNeighbor = totalEmbedding.embeddingFor(neighbor).entryFor(v)
      val previousEdge = (whiteEntryInNeighbor.previous.adjacentVertex, neighbor)
      val nextEdge = (neighbor, whiteEntryInNeighbor.next.adjacentVertex)

      if (orientedTowardsBlack(previousEdge, nextEdge, t1Edges, t2Edges))
        oriented = oriented + (UnorderedPair(v, neighbor) -> neighbor)
      else if (orientedTowardsWhite(previousEdge, nextEdge, t1Edges, t2Edges))
        oriented = oriented + (UnorderedPair(v, neighbor) -> v)
      else {
        /* This is a special case that only occurs when one of the edges are adjacent to the outer face, and thus not a
         * member of T1 or T2. In that case the white vertices out-degree is useless, but the edge is oriented to the
         * black (outer) vertex anyway, giving the white vertex degree 2 here. Only 4 such cases are present in each
         * map.
         */
        oriented = oriented + (UnorderedPair(v, neighbor) -> neighbor)

        // This error will be a pain in the ### to debug if it occurs, so let's just put a check here since it's fast.
        // The check is 8 rather than 4, since each face has two outer vertices with edges to the white vertex in it.
        outerEdgeCount += 1
        require(outerEdgeCount <= 8, "Too many white vertices with out-degree 2 added.")
      }
    }

    oriented
  }

  /**
   * @param e1 The incoming edge on the face of a black vertex in the PTP graph.
   * @param e2 The outgoing edge on the face of a black vertex in the PTP graph.
   */
  private def orientedTowardsBlack(e1: (AngularVertex[V], AngularVertex[V]),
                                   e2: (AngularVertex[V], AngularVertex[V]),
                                   t1: Set[UnorderedPair[AngularVertex[V]]],
                                   t2: Set[UnorderedPair[AngularVertex[V]]]): Boolean = {
    val ue1 = UnorderedPair(e1)
    val ue2 = UnorderedPair(e2)

    (t1.contains(ue1) && t1.contains(ue2)) || (t2.contains(ue1) && t2.contains(ue2))
  }

  /**
   * @param e1 The incoming edge on the face of a black vertex in the PTP graph.
   * @param e2 The outgoing edge on the face of a black vertex in the PTP graph.
   */
  private def orientedTowardsWhite(e1: (AngularVertex[V], AngularVertex[V]),
                                   e2: (AngularVertex[V], AngularVertex[V]),
                                   t1: Set[UnorderedPair[AngularVertex[V]]],
                                   t2: Set[UnorderedPair[AngularVertex[V]]]): Boolean = {
    val ue1 = UnorderedPair(e1)
    val ue2 = UnorderedPair(e2)

    (t1.contains(ue1) && t2.contains(ue2)) || (t2.contains(ue1) && t1.contains(ue2))
  }
}
