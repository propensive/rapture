import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import sbtrelease.ReleaseStep
import sbtrelease.ReleasePlugin.ReleaseKeys.releaseProcess
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._

enablePlugins(GitBranchPrompt)

lazy val buildSettings = Seq(
  organization := "com.propensive",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.11.7", "2.10.5")
)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
 /*   "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yinline-warnings",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture" */
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11)) => Seq("-Ywarn-unused-import")
    case _             => Seq.empty
  }),
  scalacOptions in (Compile, console) ~= (_ filterNot (_ == "-Ywarn-unused-import")),
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  scalaJSStage in Test := FastOptStage,
  scmInfo := Some(ScmInfo(url("https://github.com/propensive/rapture"),
    "scm:git:git@github.com:propensive/rapture.git")),
  commands += gitSnapshots
) ++ scalaMacroDependencies

lazy val raptureSettings = buildSettings ++ commonSettings ++ publishSettings ++ releaseSettings

lazy val rapture = project.in(file("."))
  .settings(moduleName := "root")
  .settings(raptureSettings)
  .settings(noPublishSettings)
  .settings(noSourceSettings)
  .aggregate(raptureJVM, raptureJS)
  .dependsOn(raptureJVM, raptureJS)

lazy val raptureJVM = project.in(file(".raptureJVM"))
  .settings(moduleName := "rapture")
  .settings(raptureSettings)
  .aggregate(baseJVM, coreJVM, uriJVM)
  .dependsOn(baseJVM, coreJVM, uriJVM)
  
lazy val raptureJS = project.in(file(".raptureJS"))
  .settings(moduleName := "rapture")
  .settings(raptureSettings)
  .aggregate(baseJS, coreJS, uriJS)
  .dependsOn(baseJS, coreJS, uriJS)
  .enablePlugins(ScalaJSPlugin)

lazy val base = crossProject
  .settings(moduleName := "rapture-base")
  .settings(raptureSettings:_*)
  .settings( crossVersionSharedSources():_*)

lazy val baseJVM = base.jvm
lazy val baseJS = base.js

lazy val core = crossProject.dependsOn(base)
  .settings(moduleName := "rapture-core")
  .settings(raptureSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val uri = crossProject.dependsOn(base, core)
  .settings(moduleName := "rapture-uri")
  .settings(raptureSettings:_*)
 
lazy val uriJVM = uri.jvm
lazy val uriJS = uri.js

lazy val publishSettings = Seq(
  homepage := Some(url("http://rapture.io/")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  autoAPIMappings := true,
  publishMavenStyle := true,
  publishArtifact in packageDoc := false,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <developers>
      <developer>
        <id>propensive</id>
        <name>Jon Petty</name>
        <url>http://github.com/propensive/rapture</url>
      </developer>
    </developers>
  ),
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishSignedArtifacts,
    setNextVersion,
    commitNextVersion,
    pushChanges
  )
)

lazy val publishSignedArtifacts = ReleaseStep(
  action = { st =>
    val extracted = st.extract
    val ref = extracted.get(thisProjectRef)
    extracted.runAggregated(publishSigned in Global in ref, st)
  },
  check = { st =>
    // getPublishTo fails if no publish repository is set up.
    val ex = st.extract
    val ref = ex.get(thisProjectRef)
    Classpaths.getPublishTo(ex.get(publishTo in Global in ref))
    st
  },
  enableCrossBuild = true
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val noSourceSettings = Seq(
  sources in Compile := Seq(),
  sources in Test := Seq()
)


import java.io.File

def crossVersionSharedSources()  = Seq( 
 (unmanagedSourceDirectories in Compile) ++= { (unmanagedSourceDirectories in Compile ).value.map {
     dir:File => new File(dir.getPath + "_" + scalaBinaryVersion.value)}}
)

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
              "org.scalamacros" %% "quasiquotes" % "2.1.0-M5" cross CrossVersion.binary
        )
    }
  }
)

def gitSnapshots = Command.command("gitSnapshots") { state =>
  val extracted = Project extract state
  val newVersion = Seq(version in ThisBuild := git.gitDescribedVersion.value.get + "-SNAPSHOT")
  extracted.append(newVersion, state)
}

// For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
