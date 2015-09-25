package net.cyndeline.rlgraph.cartogram.rectangular.linearProgramming.linearSolver

/**
 * Contains the optimal segment coordinates for an optimization run, as well as the total error sum.
 *
 * @param segmentCoordinates Each index in the vector contains a coordinate value that should be assigned to the segment
 *                           having variableIndex == index.
 * @param sumOfErrors The total sum of all error variables that were optimized. Each error variable contained the
 *                    difference between the current size (along one axis) of the rectangle and the target size.
 */
case class OptimalResult(segmentCoordinates: Vector[Int], sumOfErrors: Long)
