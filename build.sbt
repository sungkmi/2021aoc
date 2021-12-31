lazy val V = new {
  val scala3 = "3.1.0"
  val hedgehog = "0.8.0"
}

lazy val root = project
  .in(file("."))
  .settings(
    name := "2021aoc",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := V.scala3,

    libraryDependencies += "qa.hedgehog" %% "hedgehog-minitest" % V.hedgehog,

    testFrameworks += TestFramework("minitest.runner.Framework"),
  )
