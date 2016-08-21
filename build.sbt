organization := "io.nary"

name := "espresso"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.8.0" cross CrossVersion.binary)

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % "0.11.0",
  "org.spire-math" %% "cats" % "0.3.0",
  "org.spire-math" %% "algebra" % "0.3.0",
  "com.chuusai" %% "shapeless" % "2.2.5",
  "org.typelevel" %% "cats" % "0.4.1"
)

mainClass in Test := Some("io.nary.espresso.sample.Main")
