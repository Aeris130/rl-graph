package net.cyndeline.rlgraph.cartogram.rectangular.linearProgramming.linearSolver

/**
 * Handles the construction of constraints needed for a linear cartogram optimization, as well as constructing
 * the problem model using those constraints and solving it. The user only needs to input segment data indices
 * and target area/ratio for each rectangle.
 */
trait LinearSolver {

  /**
   * Adds multiple constraint using the upper and lower segment of a rectangle:
   *
   *  - Planarity: lower segment < higher segment
   *  - Aspect ratio: (upper - lower) <= (oppositeUpper - oppositeLower) * Max(lower/upper, upper/lower)
   *  - Cartographic error: upper - lower) * (oppositeUpper - oppositeLower) <= (1 - ERR) * A(R). The variable ERR
   *  is created, and will be used (along with all other ERR-variables) as the variable to minimize during
   *  optimization. A(R) is the target area, computed using height and width.
   *
   * @param lowerSegmentIndex Index of the lower segment of the rectangle. Given the rectangle (x1,y1), (x2,y2), this
   *                          is x1 when optimizing the x axis, and y1 when optimising the y axis.
   * @param upperSegmentIndex The upper segment opposite of x1/y1.
   * @param oppositeLowerValue A fixed value for the lower segment on the axis opposite of the one being optimized.
   *                           When optimizing the x axis, this is y1 (and vice versa x1).
   * @param oppositeUpperValue A fixed value for the upper segment on the axis opposite of the one being optimized.
   * @param targetArea The optimal area of the rectangle.
   */
  def registerRectangle(lowerSegmentIndex: Int,
                        upperSegmentIndex: Int,
                        oppositeLowerValue: Int,
                        oppositeUpperValue: Int,
                        targetArea: Int): Unit

  /**
   * Registers the planarity constraint for a rectangle. Only use this instead of registerRectangle(...) if you
   * don't want to add constraints regarding aspect ratio and cartographic error.
   * @param lowerSegmentIndex Index of the lower segment of the rectangle. Given the rectangle (x1,y1), (x2,y2), this
   *                          is x1 when optimizing the x axis, and y1 when optimising the y axis.
   * @param upperSegmentIndex The upper segment opposite of x1/y1.
   */
  def registerPlanarity(lowerSegmentIndex: Int, upperSegmentIndex: Int): Unit

  /**
   * Registers that the value of one segment should always be less than another. This method should only be used
   * to handle adjacencies between adjacent rectangles. The remaining constraints are handled using registerRectangle().
   * @param segmentAIndex Segment A. Should always be less than B.
   * @param segmentBIndex Segment B. Should always be higher than A.
   */
  def registerAdjacencyConstraint(segmentAIndex: Int, segmentBIndex: Int): Unit

  /**
   * @return A vector of optimal coordinates, where each index in the vector corresponds to the variable index of a
   *         segment. If a variable index hasn't been specified, the solution will be -1.
   */
  def computeOptimalSolution: OptimalResult
}
