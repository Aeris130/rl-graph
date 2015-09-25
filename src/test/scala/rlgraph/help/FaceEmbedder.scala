package rlgraph.help

import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.face.Face

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
 * Embeds a circular face.
 */
object FaceEmbedder {
  def embeddingFrom[VType : TypeTag : ClassTag](f: Face[VType]): Embedding[VType] = {
    var embedding: Embedding[VType] = UndirectedEmbedding[VType]()
    for (edge <- f.edges)
      embedding = embedding.embed(edge._1, edge._2)

    embedding
  }
}
