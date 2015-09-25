package net.cyndeline.rlgraph.regularEdgeLabeling.altFourCycles

import net.cyndeline.rlgraph.regularEdgeLabeling.LabelEdge

/**
 * Inner and outer edges for an alternating 4-cycle.
 */
case class EdgeData[V](outer: Vector[LabelEdge[V]], inner: Vector[LabelEdge[V]])
