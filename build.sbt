import ReleaseTransformations._

// Build settings
ThisBuild / organization := "io.nary"
ThisBuild / version := "1.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.1"

ThisBuild / releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion,
  pushChanges
)

lazy val root = (project in file("."))
  .settings(
    name := "espresso",

    resolvers ++= Resolver.sonatypeOssRepos("releases") ++ Resolver.sonatypeOssRepos("snapshots"),

    // Removed kind-projector and better-monadic-for as they're not needed in Scala 3

    // Updated dependencies
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.typelevel" %% "algebra" % "2.10.0",
      "org.typelevel" %% "shapeless3-typeable" % "3.3.0",

// Test dependencies
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    ),

    Test / run / mainClass := Some("io.nary.espresso.sample.Main"),
    Test / fork := true,

    // Updated compiler options for Scala 3
    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-source:3.3",
      "-explain", // Detailed explanations of errors
      "-Ykind-projector", // Built-in kind-projector syntax
      "-Wunused:imports",
      "-Wvalue-discard"
    )
  )