package net.cyndeline.rlgraph.cartogram.rectangular.linearProgramming.linearSolver

import solver.constraints.IntConstraintFactory
import solver.search.loop.monitors.SearchMonitorFactory
import solver.variables.{IntVar, VariableFactory => VF}
import solver.{ResolutionPolicy, Solver}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Uses the Choco Solver implementation to solve the constraint problem of optimizing segments.
 *
 * Maximum coordinate value is 10000.
 *
 * @param numberOfSegments The number of expected segments to be processed.
 * @param maximumAspectRatio The highest allowed ratio of divergence between width and height for every rectangle.
 * @param backtrackLimit Sets a limit for the number of backtracks that the solver si allowed to perform in the
 *                       search tree. Speeds up search, but may cause inability to find optimal solutions.
 */
//TODO Uses Choco solver 3.2 since 3.3 requires Java8. Switch to most recent version when the rest of the library migrates to Java8 as well.
class LinearChocoSolver(numberOfSegments: Int, maximumAspectRatio: Double, backtrackLimit: Int) extends LinearSolver {
  require(maximumAspectRatio >= 1, "The maximum aspect ratio is the highest value of w/h and h/w, which is always >= 1.0 (currently " + maximumAspectRatio + ")")

  def this(numberOfSegments: Int, maximumAspectRatio: Double) = this(numberOfSegments, maximumAspectRatio, 100)

  // Contains the final solution once the optimization has been ran once.
  private val segmentVariables: Array[IntVar] = Array.fill(numberOfSegments)(null)

  /* These are only used as optimization criteria (more specifically, the solver attempts to minimize their sum), and
   * aren't needed when computing the result for the user. Order is irrelevant. */
  private val errorVariables = new ListBuffer[IntVar]()

  // Logs constraints to make debugging easier.
  private val planarConstraints = new ArrayBuffer[String]()
  private val adjacencyConstraints = new ArrayBuffer[String]()
  private val errorConstraints = new ArrayBuffer[String]()

  // Needed to be instantiated before creating variables
  private val solver = new Solver("Optimizing segment coordinates")

  /* Integer linear can be slow (really f****** slow), so unless the search is stopped after X amount of tries, even
   * 2-3 segments will keep the solver running for minutes given the large bounds used by all variables. */
  SearchMonitorFactory.limitBacktrack(solver, backtrackLimit)

  /* Since Choco solver doesn't support constraints that mix integers and real numbers, there's no way to express
   * error and ratio constraints using doubles, since they all rely on a value between 0 and 2. Instead, the left and
   * right side of each arithmetic constraint will be multiplied by a factor of 10, bringing the variables into
   * range where integers can provide enough precision to optimize drawings. This doesn't affect the segment variables
   * final values, as only auxiliary variables will be multiplied.
   */
  private val realFactor = 10

  /* The maximum/minimum bounds of a variable. A variables value cannot go above this value, or below its negative.
   * This means that the bound is also a limit on how many coordinates in the drawing that can be represented.
   * The reason that this simply isn't set to Int.MAX is because some margin is needed to avoid overflow errors when
   * multiplying with the real factor, and the target areas provided by the user.
   */
  private val variableBound = 10000

  /* Some high value. Should always be higher than variables scaled by the realFactor and the opposite-axis fixed
   * value. If a solution is infeasible, increasing this value might be a solution. */
  private val errorBound = 3000000

  /* Auxiliary variables apparently needs a unique string associated with it, or things start to bug out. */
  private var dummyIndex = 1
  private var errorIndex = 1

  // Adding constraints after the first optimization is not allowed, since the old error-sum constraint is still present.
  private var done = false

  override def registerRectangle(lowerSegmentIndex: Int, upperSegmentIndex: Int, oppositeLowerValue: Int, oppositeUpperValue: Int, targetArea: Int): Unit = {
    val fixed = oppositeUpperValue - oppositeLowerValue
    registerPlanarity(lowerSegmentIndex, upperSegmentIndex)
    addRatioConstraint(lowerSegmentIndex, upperSegmentIndex, fixed)
    addErrorConstraint(lowerSegmentIndex, upperSegmentIndex, fixed, targetArea)
  }

  override def registerPlanarity(lowerSegmentIndex: Int, upperSegmentIndex: Int): Unit = {
    addPlanarityConstraint(lowerSegmentIndex, upperSegmentIndex)
  }

  override def registerAdjacencyConstraint(segmentAIndex: Int, segmentBIndex: Int): Unit = {
    val a = variable(segmentAIndex)
    val b = variable(segmentBIndex)
    solver.post(IntConstraintFactory.arithm(a, "<", b))
    adjacencyConstraints += (a.getName + " < " + b.getName)
  }

  /**
   * @return A vector of optimal coordinates, where each index in the vector corresponds to the variable index of a
   *         segment. If a variable index hasn't been specified, the solution will be -1.
   */
  override def computeOptimalSolution: OptimalResult = {
    if (!done) {
      done = true
      val sumOfErrors = VF.bounded("Final result", 0, VF.MAX_INT_BOUND, solver)
      solver.post(IntConstraintFactory.sum(errorVariables.toArray, sumOfErrors))
      solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, sumOfErrors)

      val result = new ArrayBuffer[Int]()

      var i = 0
      while (i < segmentVariables.size) {
        val intVar = segmentVariables(i)
        val value = if (intVar != null) intVar.getValue else -1
        result += value
        i += 1
      }

      OptimalResult(result.toVector, errorVariables.map(v => v.getValue.toLong / realFactor).sum)
    } else {
     throw new Error("Cannot optimize the same solver twice.")
    }
  }


  private def addErrorConstraint(lower: Int, upper: Int, fixed: Int, targetArea: Int) {

    // Left hand side
    val aTimesFixedAxis = VF.scale(variable(lower), fixed)
    val bTimesFixedAxis = VF.scale(variable(upper), fixed)
    val aScaledByFactor = VF.scale(aTimesFixedAxis, realFactor)
    val bScaledByFactor = VF.scale(bTimesFixedAxis, realFactor)
    val negativeA = VF.minus(aScaledByFactor)
    val bMinusA = newVariableWithSameBounds(bScaledByFactor, 2)
    solver.post(IntConstraintFactory.sum(Array(bScaledByFactor, negativeA), bMinusA))

    // Right hand side
    val areaConstant = targetArea * realFactor
    val constant = areaConstant // 1 * A(R)
    val error = errorVariable()
    val negativeError = VF.minus(error)

    val upperLimit = VF.offset(error, constant) // (1 + Err) * A(R)
    val lowerLimit = VF.offset(negativeError, constant) // (1 - Err) * A(R)

    solver.post(IntConstraintFactory.arithm(bMinusA, ">=", lowerLimit))
    solver.post(IntConstraintFactory.arithm(bMinusA, "<=", upperLimit))

    errorConstraints += ("S[" + upper + "] - S[" + lower + "] >= " + targetArea + " - (Err(R) * " + targetArea + ")")
    errorConstraints += ("S[" + upper + "] - S[" + lower + "] <= " + targetArea + " + (Err(R) * " + targetArea + ")")
  }

  /**
   * @param lower x1/y1
   * @param upper x2/y2
   * @param fixed (x2 - x1) or (y2 - y1), whichever axis is currently not being processed.
   */
  private def addRatioConstraint(lower: Int, upper: Int, fixed: Int) {
    val a = variable(lower)
    val b = variable(upper)
    val aTimesFactor = VF.scale(a, realFactor)
    val bTimesFactor = VF.scale(b, realFactor)

    /* A variable representing (b - a) is created by adding the constraint that the variable should be
     * the sum of b and -a.
     *
     * (b - a) appears two times. Once as the lhs by its own, in which case it needs to be scaled by the real factor.
     * The other time on the rhs, in which case it will be scaled by the ratio which has itself already been scaled
     * by the factor. Using the original a and b in (b-a) is sufficient there.
     */
    val bMinusAScaled = newVariableWithSameBounds(bTimesFactor)
    solver.post(IntConstraintFactory.sum(Array(bTimesFactor, VF.minus(aTimesFactor)), bMinusAScaled))
    val bMinusAOriginal = newVariableWithSameBounds(b)
    solver.post(IntConstraintFactory.sum(Array(b, VF.minus(a)), bMinusAOriginal))

    // Since reals aren't allowed, the ratio is multiplies using the real factor
    val maxAspectRatio = (maximumAspectRatio * realFactor).toInt

    // Fixed * (D * realFactor)
    val oppositeTimesAspect = fixed * maxAspectRatio

    // (x|y2 - x|y1) * (D * realFactor)
    val bMinusA_lhs = VF.scale(bMinusAOriginal, maxAspectRatio)

    // Add the constraints
    solver.post(IntConstraintFactory.arithm(bMinusAScaled, "<=", oppositeTimesAspect))
    solver.post(IntConstraintFactory.arithm(bMinusA_lhs, ">=", fixed * realFactor))
  }

  private def addPlanarityConstraint(lower: Int, upper: Int) {
    val a = variable(lower)
    val b = variable(upper)
    solver.post(IntConstraintFactory.arithm(a, "<", b))
    planarConstraints += (a.getName + " < " + b.getName)
  }

  /**
   * Creates a new variable if one doesn't already exist for a variable index.
   * @return The Choco Solver IntVar corresponding to a segment entry.
   */
  private def variable(index: Int): IntVar = {
    require(index < numberOfSegments, "Cannot retrieve variables for segments above the maximum segment size.")

    if (segmentVariables(index) != null)
      segmentVariables(index)
    else {
      val newV = newVariable("S[" + index + "]")
      segmentVariables(index) = newV
      solver.post(IntConstraintFactory.arithm(newV, ">=", 0))
      newV
    }
  }

  private def newVariable(str: String): IntVar = VF.bounded(str, -variableBound, variableBound, solver)
  private def newVariableWithSameBounds(sameBoundsAs: IntVar, multi: Int = 1): IntVar = {
    val newVariable = VF.bounded("Dummy[" + dummyIndex + "]", sameBoundsAs.getLB * multi, sameBoundsAs.getUB * multi, solver)
    dummyIndex += 1
    newVariable
  }
  private def errorVariable() = {

    /* Lower bound can't be set to 0 despite that errors can't be less than that, since the Minus of an error then
     * goes from the negative previous upper bound, to 0. Errors also can't use the regular variable bound, as
     * there's no telling how much those variables will scale, thus requiring the error to scale as well.
     */
    val newVariable = VF.bounded("Err[" + errorIndex + "]", -errorBound, errorBound, solver)
    solver.post(IntConstraintFactory.arithm(newVariable, ">=", 0))
    errorVariables += newVariable
    errorIndex += 1
    newVariable
  }

  override def toString: String = {
    val builder = new StringBuilder()
    val nl = System.getProperty("line.separator")

    builder ++= "Linear solver using Choco solver as base" + nl
    builder ++= numberOfSegments + " segments" + nl
    builder ++= "Max aspect ratio divergence: " + maximumAspectRatio + nl

    builder ++= "# Error constraints:" + nl
    for (error <- errorConstraints)
      builder ++= error + nl

    builder ++= "# Adjacency constraints" + nl
    for (adj <- adjacencyConstraints)
      builder ++= adj + nl

    builder ++= "# Planarity constraints" + nl
    for (p <- planarConstraints)
      builder ++= p + nl

    builder.toString()
  }

}
