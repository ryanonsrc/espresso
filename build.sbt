// Build settings
ThisBuild / organization := "io.nary"
ThisBuild / scalaVersion := "2.13.12"
ThisBuild / publishMavenStyle := true

lazy val root = (project in file("."))
  .settings(
    name := "espresso",

    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),

    // Modern compiler plugins
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),

    // Publishing settings
    Test / publishArtifact := false,
    homepage := Some(url("https://github.com/scommon/")),

    // POM extras preserved from original
    pomExtra := (
      <scm>
        <url>git@github.com:ryanonsrc/espresso.git</url>
        <connection>scm:git:git@github.com:ryanonsrc/espresso.git</connection>
      </scm>
        <licenses>
          <license>
            <name>Apache 2</name>
            <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <developers>
          <developer>
            <id>Ryan Delucchi</id>
            <name>Ryan Delucchi</name>
            <email>q@onsrc.com</email>
            <url>http://nary.io</url>
            <organization>nary.io</organization>
            <organizationUrl>http://nary.io</organizationUrl>
            <roles>
              <role>Architect</role>
            </roles>
          </developer>
        </developers>
      ),

    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },

    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),

    releasePublishArtifactsAction := PgpKeys.publishSigned.value,

    // Updated dependencies
    libraryDependencies ++= Seq(
      "org.typelevel" %% "spire" % "0.18.0",
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
      "-language:higherKinds",
      "-Wunused:imports",
      "-Wvalue-discard"
    )
  )