import sbt.Keys._
import sbt._

object RlgraphBuild extends Build {
  lazy val buildProject = Project("RLGraph", file(".")) settings(
    version       := "1.0",
    scalaVersion  := "2.11.8",
    scalacOptions := Seq("-deprecation"),
    name          := "rlgraph",
    organization  := "net.cyndeline",

    // Needed to access common classes
    resolvers += Resolver.mavenLocal,

	  // Resolver for the archery R*Tree repository.
    resolvers += "bintray/meetup" at "http://dl.bintray.com/meetup/maven",

    // Choco solver (v3.2.0) repository for linear programming, since the main repository only has version 3.3.
    resolvers += "choco.repos" at "http://www.emn.fr/z-info/choco-repo/mvn/repository/",
	
    libraryDependencies ++= Seq(

      "net.cyndeline" % "rlcommon_2.11" % "1.0",

	    "org.scala-lang" % "scala-reflect" % scalaVersion.value,

      // Scala Graph
      "com.assembla.scala-incubator" % "graph-core_2.11" % "1.9.4",
      "org.jgrapht" % "jgrapht-core" % "0.9.0",

      // Dependency injection
      "com.escalatesoft.subcut" % "subcut_2.10" % "2.0",

      // Linear programming solver
      "choco" % "choco-solver" % "3.2.0",

      // R*Trees
      "com.meetup" %% "archery" % "0.4.0",
	
      // Tests

      "org.scalamock" %% "scalamock-scalatest-support" % "3.1.4" % "test",
      "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
    )
  )
}
