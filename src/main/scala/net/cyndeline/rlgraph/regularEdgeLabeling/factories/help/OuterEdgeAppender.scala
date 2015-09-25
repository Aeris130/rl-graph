package net.cyndeline.rlgraph.regularEdgeLabeling.factories.help

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.face.FaceComputation
import net.cyndeline.rlgraph.util.GraphCommons

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
 * Adds the edges South -> North and West -> East to embeddings of G1 and G2.
 */
class OuterEdgeAppender[V : TypeTag : ClassTag](vN: V, vS: V, vW: V, vE: V) {
  private val faceComp = new FaceComputation[V]()

  /**
   * Embeds the edge vS -> vN in the outer face (S>W>N>E) of G1's embedding.
   * @param e The embedding containing only vertices and edges of G1.
   */
  def addExtraG1Edge(e: Embedding[V]): Embedding[V] = {
    val outerVSet = Set(vN, vS, vW, vE)
    val outer = faceComp.computeFaces(e).find(f => {
      val vSet = f.vertices.toSet
      vSet == outerVSet
    }).getOrElse(throw new Error("No outer face with only outer vertices in G1."))

    GraphCommons.embedEdgeInFace(e, outer, vS, vN)
  }

  /**
   * Embeds the edge vW -> vE in the outer face (S>W>N>E) of G1's embedding.
   * @param e The embedding containing only vertices and edges of G2.
   */
  def addExtraG2Edge(e: Embedding[V]): Embedding[V] = {
    val outerVSet = Set(vN, vS, vW, vE)
    val outer = faceComp.computeFaces(e).find(f => {
      val vSet = f.vertices.toSet
      vSet == outerVSet
    }).getOrElse(throw new Error("No outer face with only outer vertices in G2."))

    GraphCommons.embedEdgeInFace(e, outer, vW, vE)
  }

  /**
   * Creates a new embedding based on the embedding used during the creation of T1/T2, containing only the edges
   * found in either T1 or T2. Also keeps the edges between the outer vertices, as they're needed anyway when
   * constructing the dual using this embeddings faces.
   * @param t The list of edges that should remain in the embedding. Either T1 or T2s edges.
   * @param e The embedding to trim.
   */
  def trimEmbedding(t: Vector[(V, V)], e: Embedding[V]): Embedding[V] = {
    var newEmbedding = e

    val tSet = t.toSet.map((e: (V, V)) => UnorderedPair(e)) + UnorderedPair(vS, vW) + UnorderedPair(vW, vN) + UnorderedPair(vN, vE) + UnorderedPair(vE, vS)
    for (edge <- newEmbedding.edges if !tSet.contains(UnorderedPair(edge)))
      newEmbedding = newEmbedding.deleteEdge(edge._1, edge._2)

    newEmbedding
  }

}
