package net.cyndeline.rlgraph.cartogram.rectangular.linearProgramming

import net.cyndeline.rlcommon.util.{HeightConstraint, WidthConstraint}
import net.cyndeline.rlgraph.cartogram.rectangular.common.{Constraint, _}
import net.cyndeline.rlgraph.cartogram.rectangular.linearProgramming.linearSolver.{LinearChocoSolver, LinearSolver}
import net.cyndeline.rlgraph.drawings.planar.rectangular.RectangularLayout
import net.cyndeline.rlgraph.drawings.planar.rectangular.dataStructure.Rectangle

import scalax.collection.GraphEdge.UnDiEdge

/**
 * A note to all users: Before wasting your time with this class, be aware that computing an optimal cartogram
 * is NP-hard. It is not recommended for use on layouts with size > ~10. A faster alternative is the segment heuristic,
 * also available in the cartogram package.
 *
 * This is an optimization using linear programming to compute a cartogram from a rectangular layout with low
 * cartographic error. The cartographic error of a rectangular area in the optimization is considered to be
 * |A - As(R)| / As(R), where A is the actual area of the rectangle and As(R) is the optimal area targeted by the user.
 *
 * The algorithm is described in the article "A Linear Programming Approach to Rectangular Cartograms" by Bettina
 * Speckmann, Marc van Kreveld and Sander Florisson. Note that the purpose of the algorithm originally was to
 * compute an accurate representation of a geographical map. Due to this, the original algorithm contains a step
 * where the initial rectangular dual is chosen such that it contains areas with positions that mimics said map, in
 * order to make comparisons between the map and the optimized rectangular layout easier for users. Since no such
 * requirement exists for an arbitrary dual (one not based on a geographic map), this step has been omitted.
 *
 * Instead, the focus is solely on optimizing the following criteria:
 *
 *  1. Width and height of each individual rectangle, such that they correspond to the width/height of the vertices
 *  in the original input graph. Diverging from the targeted area is referred to as the cartographic error, and can be
 *  defined as |area - targeted_area| / targeted_area.
 *  1. Aspect ratio of each rectangle being at or below a user-specified maximum ratio.
 *  1. Planarity: With each rectangle being bounded by two segments on the x axis and two segments on the y axis,
 *  the lower segment on each axis may never be equal or higher than the higher segment (this would result in an
 *  inverted area or an area of width/height 0).
 *  1. Adjacency: Optimization may only occur insofar that the rectangles adjacent in the original layout still remains
 *  adjacent in the optimal solution.
 *
 * A note on adjacency: Since computing the rectangular dual involves biconnecting and triangulating an input graph
 * in order to guarantee the existence of a dual, it is possible that the dual contains adjacent rectangles represented
 * by vertices that are not adjacent in the original graph. The user may specify that such adjacencies can be
 * disregarded, leaving more space for the algorithm to optimize the remaining adjacent areas.
 *
 * Selecting the maximum divergence in aspect ratios has a large impact on how well the size error may be optimized,
 * as the ratio effectively bounds how far the height/width may diverge. It is important to remember that the algorithm
 * works iteratively: First the x coordinates of the drawing are optimized by using the y coordinates as fixed values.
 * After that, the same procedure is repeated with the y coordinates. Optimizing coordinates on one axis may result in
 * further optimization being allowed on the other axis, so this procedure is repeated until no more optimization
 * is possible. This does however mean that the maximum aspect ratio does not refer to the ratio of the final drawing,
 * but rather the divergence of ratios between the two axises at any given point in time during the algorithm.
 *
 * Ratio example: Suppose you have a rectangular area R with size 2x2, a maximum aspect ratio of 1 (meaning every
 * rectangle should be 1:1, having all four sides be the same size), and a target area of 5x5. When the algorithm
 * first tries to optimize the x axis, it checks how far the size of R's x coordinates can be increased without
 * breaking the aspect ratio. But since the ratio is 1:1, even a rectangle with size 3x2 would be illegal. Thus
 * no changes would be made to R since the y axis blocks the x axis from increasing, and vice versa. The only rectangles
 * that could be optimized given this ratio are the ones that already violates it (by decreasing the amount they exceed
 * the ratio), given that doing so doesn't cause other rectangles to in turn exceed the ratio. To prevent this from
 * happening, it is advised to avoid tight ratio limits for arbitrary drawings.
 *
 * On sea rectangles: The algorithm allows the user to specify a set of vertices representing sea rectangles, i.e
 * areas that doesn't need to be constrained by size or aspect ratios (only planarity and adjacencies).
 * The name stems from their use in the above mentioned article by Speckmann et. al, where the areas on a map that
 * represents water regions surrounding a continent doesn't necessarily need to be as strictly optimized as the
 * actual landmasses inside them. By allowing the algorithm to disregard these areas, the remaining vertices have more
 * space available to reach the desired size.
 *
 * Finally, the algorithm does not apply size or aspect ratio constraints to gates in the layout, as the user has not
 * specified that data for them.
 */
class LinearOptimization[VType <: WidthConstraint with HeightConstraint, EType[X] <: UnDiEdge[X]] {

  /**
   * @param layout A rectangular layout to optimize area and aspect ratio for.
   * @param maxAspectRatio The highest aspect ratio divergence allowed for every rectangle in the drawing.
   * @return A new rectangular layout with x and y coordinates optimized, or the input layout if no optimization
   *         was possible.
   */
  def optimizeLayout(layout: RectangularLayout[VType, EType], maxAspectRatio: Double): RectangularLayout[VType, EType] = optimizeLayoutWithExceptions(layout, maxAspectRatio, Set())

  /**
   * @param layout A rectangular layout to optimize area and aspect ratio for.
   * @param maxAspectRatio The highest aspect ratio divergence allowed for every rectangle in the drawing.
   * @param seaRectangles Vertices representing rectangles in the layout that doesn't need to be constrained by their
   *                      target size or aspect ratio.
   * @return A new rectangular layout with x and y coordinates optimized, or the input layout if no optimization
   *         was possible.
   */
  def optimizeLayoutWithExceptions(layout: RectangularLayout[VType, EType], maxAspectRatio: Double, seaRectangles: Set[VType]): RectangularLayout[VType, EType] = {
    val segmentMap = new SegmentMap(layout)

    // Only needs to be computed once
    val verticalAdjacencies = adjacencyConstraints(layout, segmentMap, X_Axis)
    val horizontalAdjacencies = adjacencyConstraints(layout, segmentMap, Y_Axis)

    // Make sure that the rectangles to optimize stays in constant order from now on
    val rectangles: Vector[(VType, Rectangle)] = layout.rectangles.toVector

    val verticalSegmentAmount = segmentMap.verticalSegments.size
    val horizontalSegmentAmount = segmentMap.horizontalSegments.size

    var previousErrorX: Option[Long] = None
    var previousErrorY: Option[Long] = None

    /* Algorithm stops when neither axis can improve its cartographic error. */
    var xErrorImproved = true
    var yErrorImproved = true

    while (xErrorImproved || yErrorImproved) {

      // Optimize vertical segments
      val solverX: LinearSolver = new LinearChocoSolver(verticalSegmentAmount, maxAspectRatio)
      registerRectangles(X_Axis, solverX, rectangles, segmentMap, seaRectangles)
      addAdjacencyConstraints(solverX, verticalAdjacencies)
      val solutionX = solverX.computeOptimalSolution

      if (previousErrorX.isEmpty || previousErrorX.get > solutionX.sumOfErrors) {
        xErrorImproved = true
        previousErrorX = Some(solutionX.sumOfErrors)
        updateSegments(X_Axis, segmentMap, solutionX.segmentCoordinates)
      } else {
        xErrorImproved = false
      }

      // Optimize horizontal segments
      val solverY: LinearSolver = new LinearChocoSolver(horizontalSegmentAmount, maxAspectRatio)
      registerRectangles(Y_Axis, solverY, rectangles, segmentMap, seaRectangles)
      addAdjacencyConstraints(solverY, horizontalAdjacencies)
      val solutionY = solverY.computeOptimalSolution

      if (previousErrorY.isEmpty || previousErrorY.get > solutionY.sumOfErrors) {
        yErrorImproved = true
        previousErrorY = Some(solutionY.sumOfErrors)
        updateSegments(Y_Axis, segmentMap, solutionY.segmentCoordinates)
      } else {
        yErrorImproved = false
      }
    }

    if (previousErrorX.isEmpty && previousErrorY.isEmpty)
      layout
    else
      segmentMap.constructNewDrawingFromSegments
  }


  private def updateSegments(axis: Axis, segmentMap: SegmentMap[VType, EType], newValues: Vector[Int]): Unit = {
    val segments = if (axis == X_Axis) segmentMap.verticalSegments else segmentMap.horizontalSegments
    for (s <- segments) {
      s.value = newValues(s.variableIndex)
    }
  }

  private def registerRectangles(axis: Axis,
                                 solver: LinearSolver,
                                 rectangles: Vector[(VType, Rectangle)],
                                 segmentMap: SegmentMap[VType, EType],
                                 seaRectangles: Set[VType]) {
    for (r <- rectangles) {
      val s = segmentMap.segmentOfArea(r._2)
      val targetArea = (r._1.elementHeight - 1) * (r._1.elementWidth - 1)

      if (!seaRectangles.contains(r._1)) {
        axis match {
          case X_Axis => solver.registerRectangle(s.left.variableIndex, s.right.variableIndex, s.top.value, s.bottom.value, targetArea)
          case Y_Axis => solver.registerRectangle(s.top.variableIndex, s.bottom.variableIndex, s.left.value, s.right.value, targetArea)
        }
      } else {
        axis match {
          case X_Axis => solver.registerPlanarity(s.left.variableIndex, s.right.variableIndex)
          case Y_Axis => solver.registerPlanarity(s.top.variableIndex, s.bottom.variableIndex)
        }
      }

    }
  }

  private def addAdjacencyConstraints(solver: LinearSolver, constraints: Vector[Constraint]) {
    for (c <- constraints) {
      val lower = if (c.relation == LessThan) c.other.variableIndex else c.segment.variableIndex
      val upper = if (c.segment.variableIndex == lower) c.other.variableIndex else c.segment.variableIndex
      solver.registerAdjacencyConstraint(lower, upper)
    }
  }

  /**
   * Every rectangle in the original layout must remain adjacent to its neighbors that doesn't share one of its two
   * edges along the axis being computed. See the AdjacencyConstraints class for more information.
   */
  private def adjacencyConstraints(layout: RectangularLayout[VType, EType], segments: SegmentMap[VType, EType], axis: Axis): Vector[Constraint] = {
    (for {
      rectangle <- layout.allAreas
      if layout.adjacentAreas contains rectangle
      neighbors = layout.adjacentAreas(rectangle)
      rectSegments = segments.segmentOfArea(rectangle)
      neighborSegments = neighbors.map(segments.segmentOfArea)
      newConstraints = new AdjacencyConstraints(rectSegments, neighborSegments, axis).constraints
    } yield newConstraints).flatten
  }

}
