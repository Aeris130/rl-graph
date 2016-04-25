package net.cyndeline.rlgraph.canonicalOrder

import net.cyndeline.rlgraph.embedding.Embedding

/**
  * Specifies methods used to construct canonical orderings.
  */
trait CanonicalOrder[V] {

  /**
    * @param embedding A planar undirected embedding.
    * @return The vertices in the graph, in the order they appear in the canonical ordering.
    */
  def order(embedding: Embedding[V]): Vector[V]

}
