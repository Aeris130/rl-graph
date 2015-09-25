package net.cyndeline.rlgraph.spqr

/**
 * A solid or virtual edge in an SPQR node.
 */
trait Edge[VType] {
  val from: VType
  val to: VType
}
