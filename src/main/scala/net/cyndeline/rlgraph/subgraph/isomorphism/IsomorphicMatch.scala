package net.cyndeline.rlgraph.subgraph.isomorphism

/**
 * The resulting pattern-node -> graph-node and pattern-edge -> graph-edge mapping that results from a successful
 * isomorphic match.
 */
class IsomorphicMatch[N1, N2](val nodes: Map[N2, N1]) {

  override def equals(other: Any): Boolean = other match {
    case im: IsomorphicMatch[N1, N2] => nodes == im.nodes
    case _ => false
  }

  override def hashCode: Int = nodes.##

  override def toString: String = {
    val builder = new StringBuilder()
    builder ++= String.format("Nodemap:%n")
    for (nPair <- nodes) {
      builder ++= String.format(nPair._1 + " matches " + nPair._2 + "%n")
    }

    builder.toString()
  }

}
