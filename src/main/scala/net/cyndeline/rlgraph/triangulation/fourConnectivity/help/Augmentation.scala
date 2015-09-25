package net.cyndeline.rlgraph.triangulation.fourConnectivity.help

/**
 * Contains vertex tuples representing edges that should be added to an embeddings face.
 */
case class Augmentation[V](edges: Vector[(V, V)])
