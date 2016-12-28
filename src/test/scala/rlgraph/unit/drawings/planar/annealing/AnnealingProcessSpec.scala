package rlgraph.unit.drawings.planar.annealing

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point}
import net.cyndeline.rlcommon.stat.Statistics
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.annealing.AnnealingProcess
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.annealing.AnnealingProcess.Settings
import rlgraph.SpecImports
import rlgraph.help.ProduceDrawing
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.DefaultState
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.costFunctions.{BorderLinesScore, DistributionScore, EdgeLengthScore}

import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

class AnnealingProcessSpec extends SpecImports {

  private def random = new Random(666) // Seeded to keep some semblance of determinism

  /* To make the tests run faster and more predictable, a single coordinate's worth
   * of movement is used as temperature.
   */
  private def singleSetting(scoreToUse: String, score: Int, targetEdgeLength: Int = 0): Settings[DefaultState] = scoreToUse match {
    case "Edge" => singleSetting(score, 0, 0, targetEdgeLength)
    case "Border" => singleSetting(0, score, 0, targetEdgeLength)
    case "Distribution" => singleSetting(0, 0, score, targetEdgeLength)
    case "All" => singleSetting(score, score, score, targetEdgeLength)
  }

  private def singleSetting(ew: Int, bw: Int, dw: Int, targetEdgeLength: Int): Settings[DefaultState] = Settings[DefaultState](
    temperature = 2, // 1 might be rounded down to 0 when moving diagonally
    cooling = 0.5,
    Vector(new EdgeLengthScore(ew, targetEdgeLength), new BorderLinesScore(bw), new DistributionScore(dw, 20))
  )

  private def buildProcess(vertices: Map[Int, Point],
                           graph: Graph[Int, UnDiEdge],
                           border: Int,
                           settings: Settings[DefaultState] = singleSetting("All", 1)) = {
    val drawing = ProduceDrawing(vertices, graph)
    val state = new DefaultState()
    AnnealingProcess(drawing, graph, border, settings, state)
  }

  // A single vertex in a border from (0,0) to (6,6). Uses border-scores only.
  private def singleVertex(point: Point) = new {
    val drawingSize = 7
    val v = (0, point)
    val graph = Graph[Int, UnDiEdge](v._1)
    val coordinates = Map(v)
    val process = buildProcess(coordinates, graph, drawingSize, singleSetting("Border", 1))
  }

  // Two vertices in a 6x6 border
  private def doubleEdgeTarget(v1P: Point, v2P: Point, targetLength: Int) = new {
    val drawingSize = 7
    val v1 = (0, v1P)
    val v2 = (1, v2P)
    val graph = Graph[Int, UnDiEdge](v1._1 ~ v2._1)
    val coordinates = Map(v1, v2)
    val process = buildProcess(coordinates, graph, drawingSize, singleSetting("Edge", 1, targetLength))
  }

  describe("AnnealingProcess") {

    /*
     * Misc
     */

    it("should do nothing with a graph having no vertices") {

      Given("an empty process")
      val process = buildProcess(Map(), Graph(), 1)

      When("running the process")
      val running = process.run(30, random)

      Then("nothing should happen")
      running should be (None)

    }

    it ("should do nothing with a graph having a single vertex") {

      Given("a graph with a single vertex")
      val v = 0
      val process = buildProcess(Map(v -> Point(3, 3)), Graph(v), 4)

      When("running the process")
      val running = process.run(30, random)

      Then("nothing should happen")
      running should be (None)

    }

    /*
     * Edge score
     */

    it("should contract an edge towards its target") {

      /* Normally there's no guarantee that an edge will reach its target, but given enough movements, it should
       * happen with a single pair of vertices and ample space.
       */

      Given("two vertices at the opposite ends of a 6x6 drawing, having an edge of length 6 and a target length 3")
      val targetLength = 3
      val f = doubleEdgeTarget(Point(0, 3), Point(6, 3), targetLength)
      import f._
      val initialLength = v1._2.distanceTo(v2._2)
      assert(initialLength > targetLength)

      When("running the process multiple times using only edge distance as score")
      val running = process.run(100, random).get

      And("the distance between both vertices should be less than the intial length")
      assert(running.coordinates(v1._1).distanceTo(running.coordinates(v2._1)) < initialLength)

    }

    it("should expand an edge towards its target") {

      Given("two vertices next to each other in a 6x6 drawing, having an edge of length 1 and a target length 4")
      val targetLength = 4
      val f = doubleEdgeTarget(Point(4, 3), Point(5, 3), targetLength)
      import f._
      val initialLength = v1._2.distanceTo(v2._2)
      assert(initialLength < targetLength)

      When("running the process multiple times using only edge distance as score")
      val running = process.run(100, random).get

      And("the distance between both vertices should be greater than the initial length")
      assert(running.coordinates(v1._1).distanceTo(running.coordinates(v2._1)) > initialLength)

    }

    it("should contract an edge as far as possible towards its target") {

      Given("a target edge length of 0, and a drawing with vertices having distance > 1 between each other")
      val targetLength = 0
      val f = doubleEdgeTarget(Point(0, 3), Point(6, 3), targetLength)
      import f._

      When("running the process multiple times using only edge distance as score")
      val running = process.run(100, random).get

      And("the distance between both vertices should be as close to 0 as possible")
      assert(running.coordinates(v1._1).distanceTo(running.coordinates(v2._1)) == 1)

    }

    /*
     * Border score
     */

    it("should move a vertex at the border inwards") {

      Given("a drawing from (0,0) to (6,6), and a vertex at (6,6)")
      val before = Point(6, 6)
      val f = singleVertex(before)
      import f._
      val center = Point(3, 3)

      When("running the process multiple times using only border distance as score")
      val running = process.run(100, random).get

      Then("the vertex should be placed closer to the center than it initially is")
      val after = running.coordinates(v._1)
      val beforeDiff = center - before
      val afterDiff = center - after
      assert(Math.abs(beforeDiff.x) > Math.abs(afterDiff.x))
      assert(Math.abs(beforeDiff.y) > Math.abs(afterDiff.y))

    }

    it("should not move a vertex already at the center of the borders despite it having space to move") {

      Given("a single-point vertex centered in a drawing")
      val f = singleVertex(Point(3, 3))
      import f._

      When("running the process multiple times using only border distance as score")
      val running = process.run(100, random)

      Then("no changes should be made to the vertex")
      running should be (None)

    }

    /*
     * Distribution score
     */

    it("should space out vertices evenly") {

      Given("three vertices unevenly distributed in a 6x6 drawing")
      val drawingSize = 7
      val v1 = (0, Point(0, 0))
      val v2 = (1, Point(1, 0))
      val v3 = (2, Point(6, 6)) // On the far opposite end of the others
      val graph = Graph[Int, UnDiEdge](v1._1 ~ v2._1, v2._1 ~ v3._1)
      val coordinates = Map(v1, v2, v3)
      val process = buildProcess(coordinates, graph, drawingSize, singleSetting("Distribution", 1))

      When("running the process multiple times using only vertex distribution as score")
      val running = process.run(1000, random).get

      Then("all three vertices should have the same distance to each other")
      // 2 coordinates marginal due to diagonals
      val d1_2 = running.coordinates(v1._1).distanceTo(running.coordinates(v2._1))
      val d1_3 = running.coordinates(v1._1).distanceTo(running.coordinates(v3._1))
      val d2_3 = running.coordinates(v2._1).distanceTo(running.coordinates(v3._1))
      assert(Statistics.stdDeviation(Vector(d1_2, d1_3, d2_3)) < 2)

    }

  }

}
