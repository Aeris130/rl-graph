package net.cyndeline.rlgraph.drawings.planar.rectangular.triangleBreak.help

/**
 * Returned from triangle-breaking algorithms. Contains all vertices selected so far as the optimal solution, and
 * also a list of bubble-graphs that hasn't been emptied yet (i.e that still has unbroken triangles).
 */
case class OptimalSolution[V](vertices: Vector[BubbleVertex[V]], graphsRemaining: Vector[BubbleGraph[V]])
