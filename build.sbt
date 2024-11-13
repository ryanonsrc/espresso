import ReleaseTransformations._

// Build settings
ThisBuild / organization := "io.nary"
ThisBuild / version := "1.0.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.12"

releaseProcess := Seq[ReleaseStep](
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

    // Modern compiler plugins
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),

    // Updated dependencies
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.typelevel" %% "algebra" % "2.10.0",
      "com.chuusai" %% "shapeless" % "2.3.10",

      // Test dependencies
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    ),

    Test / run / mainClass := Some("io.nary.espresso.sample.Main"),
    Test / fork := true,

      // Compiler options
    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-Wunused:imports",
      "-Wvalue-discard"
    )
  )