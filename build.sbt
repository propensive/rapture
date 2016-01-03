import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import ReleaseTransformations._

enablePlugins(GitBranchPrompt)

lazy val buildSettings = Seq(
  organization := "com.propensive",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.11.7", "2.10.5")
)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-Xlint",
    "-Ywarn-dead-code"
 /*   "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-Xfatal-warnings",
    "-Yinline-warnings",
    "-Yno-adapted-args",
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
  concurrentRestrictions in Global ++= Seq(Tags.limitSum(2, Tags.CPU, Tags.Untagged), Tags.limit(Tags.Test, 1)),
  scmInfo := Some(ScmInfo(url("https://github.com/propensive/rapture"),
    "scm:git:git@github.com:propensive/rapture.git"))
) ++ scalaMacroDependencies

lazy val raptureSettings = buildSettings ++ commonSettings ++ publishSettings

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
  .aggregate(baseJVM, coreJVM, timeJVM, uriJVM, codecJVM, cryptoJVM, csvJVM, ioJVM, fsJVM, netJVM, httpJVM, httpJettyJVM, mimeJVM, cliJVM, logJVM, i18nJVM, textJVM, latexJVM, testJVM, dataJVM, xmlJVM, xmlStdlibJVM, jsonJVM, htmlJVM, domJVM, jsonJawnJVM, jsonPlayJVM, jsonSprayJVM, jsonJson4sJVM, jsonCirceJVM, jsonArgonautJVM, jsonJacksonJVM, /*jsonLiftJVM, */coreScalazJVM, coreTestJVM, i18nTestJVM, cliTestJVM, jsonTestJVM, xmlTestJVM)
  .dependsOn(baseJVM, coreJVM, timeJVM, uriJVM, codecJVM, cryptoJVM, csvJVM, ioJVM, fsJVM, netJVM, httpJVM, httpJettyJVM, mimeJVM, cliJVM, logJVM, i18nJVM, textJVM, latexJVM, testJVM, dataJVM, xmlJVM, xmlStdlibJVM, jsonJVM, htmlJVM, domJVM, jsonJawnJVM, jsonPlayJVM, jsonSprayJVM, jsonJson4sJVM, jsonCirceJVM, jsonArgonautJVM, jsonJacksonJVM, /*jsonLiftJVM, */coreScalazJVM, coreTestJVM, i18nTestJVM, cliTestJVM, jsonTestJVM, xmlTestJVM)
  
lazy val raptureJS = project.in(file(".raptureJS"))
  .settings(moduleName := "rapture")
  .settings(raptureSettings)
  .aggregate(baseJS, coreJS, timeJS, uriJS, codecJS, cryptoJS, csvJS, ioJS, fsJS, netJS, httpJS, httpJettyJS, mimeJS, cliJS, logJS, i18nJS, textJS, latexJS, testJS, dataJS, xmlJS, xmlStdlibJS, jsonJS, htmlJS, domJS, jsonJawnJS, /*jsonLiftJS, */jsonPlayJS, jsonSprayJS, jsonJson4sJS, jsonCirceJS, jsonArgonautJS, jsonJacksonJS, coreScalazJS, coreTestJS, i18nJS, cliTestJS, coreTestJS, i18nTestJS, jsonTestJS, xmlTestJS)
  .dependsOn(baseJS, coreJS, timeJS, uriJS, codecJS, cryptoJS, csvJS, ioJS, fsJS, netJS, httpJS, httpJettyJS, mimeJS, cliJS, logJS, i18nJS, textJS, latexJS, testJS, dataJS, xmlJS, xmlStdlibJS, jsonJS, htmlJS, domJS, jsonJawnJS, /*jsonLiftJS, */jsonPlayJS, jsonSprayJS, jsonJson4sJS, jsonCirceJS, jsonArgonautJS, jsonJacksonJS, coreScalazJS, coreTestJS, i18nJS, cliTestJS, coreTestJS, i18nTestJS, jsonTestJS, xmlTestJS)
  .enablePlugins(ScalaJSPlugin)

// rapture-base
lazy val base = crossProject
  .settings(moduleName := "rapture-base")
  .settings(raptureSettings:_*)
  .settings(crossVersionSharedSources():_*)

lazy val baseJVM = base.jvm
lazy val baseJS = base.js

// rapture-core
lazy val core = crossProject.dependsOn(base)
  .settings(moduleName := "rapture-core")
  .settings(raptureSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

// rapture-uri
lazy val uri = crossProject.dependsOn(core)
  .settings(moduleName := "rapture-uri")
  .settings(raptureSettings:_*)
 
lazy val uriJVM = uri.jvm
lazy val uriJS = uri.js

// rapture-codec
lazy val codec = crossProject.dependsOn(core)
  .settings(moduleName := "rapture-codec")
  .settings(raptureSettings:_*)
 
lazy val codecJVM = codec.jvm
lazy val codecJS = codec.js

// rapture-crypto
lazy val crypto = crossProject.dependsOn(core, codec)
  .settings(moduleName := "rapture-crypto")
  .settings(raptureSettings:_*)
 
lazy val cryptoJVM = crypto.jvm
lazy val cryptoJS = crypto.js

// rapture-io
lazy val io = crossProject.dependsOn(codec, mime, uri)
  .settings(moduleName := "rapture-io")
  .settings(raptureSettings:_*)
 
lazy val ioJVM = io.jvm
lazy val ioJS = io.js

// rapture-mime
lazy val mime = crossProject.dependsOn()
  .settings(moduleName := "rapture-mime")
  .settings(raptureSettings:_*)
 
lazy val mimeJVM = mime.jvm
lazy val mimeJS = mime.js

// rapture-net
lazy val net = crossProject.dependsOn(io)
  .settings(moduleName := "rapture-net")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "commons-net" % "commons-net" % "2.0")
 
lazy val netJVM = net.jvm
lazy val netJS = net.js

// rapture-time
lazy val time = crossProject.dependsOn(core)
  .settings(moduleName := "rapture-time")
  .settings(raptureSettings:_*)
 
lazy val timeJVM = time.jvm
lazy val timeJS = time.js

// rapture-http
lazy val http = crossProject.dependsOn(net, uri, json, html, fs, log, time)
  .settings(moduleName := "rapture-http")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "javax.servlet" % "servlet-api" % "2.5")
 
lazy val httpJVM = http.jvm
lazy val httpJS = http.js

// rapture-http-jetty
lazy val `http-jetty` = crossProject.dependsOn(http)
  .settings(moduleName := "rapture-http-jetty")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "org.eclipse.jetty" % "jetty-servlet" % "7.6.10.v20130312")
 
lazy val httpJettyJVM = `http-jetty`.jvm
lazy val httpJettyJS = `http-jetty`.js

// rapture-fs
lazy val fs = crossProject.dependsOn(io)
  .settings(moduleName := "rapture-fs")
  .settings(raptureSettings:_*)
 
lazy val fsJVM = fs.jvm
lazy val fsJS = fs.js

// rapture-csv
lazy val csv = crossProject.dependsOn(fs)
  .settings(moduleName := "rapture-csv")
  .settings(raptureSettings:_*)
 
lazy val csvJVM = csv.jvm
lazy val csvJS = csv.js

// rapture-cli
lazy val cli = crossProject.dependsOn(log, fs)
  .settings(moduleName := "rapture-cli")
  .settings(raptureSettings:_*)
 
lazy val cliJVM = cli.jvm
lazy val cliJS = cli.js

// rapture-log
lazy val log = crossProject.dependsOn(io)
  .settings(moduleName := "rapture-log")
  .settings(raptureSettings:_*)
 
lazy val logJVM = log.jvm
lazy val logJS = log.js

// rapture-i18n
lazy val i18n = crossProject.dependsOn(core)
  .settings(moduleName := "rapture-i18n")
  .settings(raptureSettings:_*)
 
lazy val i18nJVM = i18n.jvm
lazy val i18nJS = i18n.js

// rapture-text
lazy val text = crossProject.dependsOn(core)
  .settings(moduleName := "rapture-text")
  .settings(raptureSettings:_*)
 
lazy val textJVM = text.jvm
lazy val textJS = text.js

// rapture-latex
lazy val latex = crossProject.dependsOn(text, cli)
  .settings(moduleName := "rapture-latex")
  .settings(raptureSettings:_*)
 
lazy val latexJVM = latex.jvm
lazy val latexJS = latex.js

// rapture-test
lazy val test = crossProject.dependsOn(cli, fs, text)
  .settings(moduleName := "rapture-test")
  .settings(raptureSettings:_*)
 
lazy val testJVM = test.jvm
lazy val testJS = test.js

// rapture-dom
lazy val dom = crossProject.dependsOn(core)
  .settings(moduleName := "rapture-dom")
  .settings(raptureSettings:_*)
 
lazy val domJVM = dom.jvm
lazy val domJS = dom.js

// rapture-html
lazy val html = crossProject.dependsOn(net, mime, dom)
  .settings(moduleName := "rapture-html")
  .settings(raptureSettings:_*)
 
lazy val htmlJVM = html.jvm
lazy val htmlJS = html.js

// rapture-data
lazy val data = crossProject.dependsOn(core)
  .settings(moduleName := "rapture-data")
  .settings(raptureSettings:_*)
 
lazy val dataJVM = data.jvm
lazy val dataJS = data.js

// rapture-xml
lazy val xml = crossProject.dependsOn(data)
  .settings(moduleName := "rapture-xml")
  .settings(raptureSettings:_*)
 
lazy val xmlJVM = xml.jvm
lazy val xmlJS = xml.js

// rapture-json
lazy val json = crossProject.dependsOn(data)
  .settings(moduleName := "rapture-json")
  .settings(raptureSettings:_*)
 
lazy val jsonJVM = json.jvm
lazy val jsonJS = json.js

// rapture-json-circe
lazy val `json-circe` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-circe")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "io.circe" %% "circe-core" % "0.2.1")
  .settings(libraryDependencies += "io.circe" %% "circe-jawn" % "0.2.1")
 
lazy val jsonCirceJVM = `json-circe`.jvm
lazy val jsonCirceJS = `json-circe`.js

// rapture-xml-stdlib
lazy val `xml-stdlib` = crossProject.dependsOn(xml)
  .settings(moduleName := "rapture-xml-stdlib")
  .settings(raptureSettings:_*)
 
lazy val xmlStdlibJVM = `xml-stdlib`.jvm
lazy val xmlStdlibJS = `xml-stdlib`.js

// rapture-json-jawn
lazy val `json-jawn` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-jawn")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "org.spire-math" %% "jawn-parser" % "0.8.2")
  .settings(libraryDependencies += "org.spire-math" %% "jawn-ast" % "0.8.2")
 
lazy val jsonJawnJVM = `json-jawn`.jvm
lazy val jsonJawnJS = `json-jawn`.js


// rapture-json-lift
lazy val `json-lift` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-lift")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "net.liftweb" %% "lift-json" % "2.6.2")
 
lazy val jsonLiftJVM = `json-lift`.jvm
lazy val jsonLiftJS = `json-lift`.js

// rapture-json-play
lazy val `json-play` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-play")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "com.typesafe.play" %% "play-json" % "2.4.2")
 
lazy val jsonPlayJVM = `json-play`.jvm
lazy val jsonPlayJS = `json-play`.js

// rapture-json-json4s
lazy val `json-json4s` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-json4s")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "org.json4s" %% "json4s-native" % "3.3.0.RC3")
 
lazy val jsonJson4sJVM = `json-json4s`.jvm
lazy val jsonJson4sJS = `json-json4s`.js

// rapture-json-spray
lazy val `json-spray` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-spray")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "io.spray" %% "spray-json" % "1.3.2")
 
lazy val jsonSprayJVM = `json-spray`.jvm
lazy val jsonSprayJS = `json-spray`.js

// rapture-json-argonaut
lazy val `json-argonaut` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-argonaut")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "io.argonaut" %% "argonaut" % "6.1")
 
lazy val jsonArgonautJVM = `json-argonaut`.jvm
lazy val jsonArgonautJS = `json-argonaut`.js

// rapture-json-jackson
lazy val `json-jackson` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-jackson")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.6.3")
 
lazy val jsonJacksonJVM = `json-jackson`.jvm
lazy val jsonJacksonJS = `json-jackson`.js

// rapture-core-scalaz
lazy val `core-scalaz` = crossProject.dependsOn(core)
  .settings(moduleName := "rapture-core-scalaz")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.3")
  .settings(libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.1.3")
 
lazy val coreScalazJVM = `core-scalaz`.jvm
lazy val coreScalazJS = `core-scalaz`.js

// rapture-core-test
lazy val `core-test` = crossProject.dependsOn(core, `core-scalaz`, test)
  .settings(moduleName := "rapture-core-test")
  .settings(raptureSettings:_*)
 
lazy val coreTestJVM = `core-test`.jvm
lazy val coreTestJS = `core-test`.js

// rapture-i18n-test
lazy val `i18n-test` = crossProject.dependsOn(i18n, test)
  .settings(moduleName := "rapture-i18n-test")
  .settings(raptureSettings:_*)
 
lazy val i18nTestJVM = `i18n-test`.jvm
lazy val i18nTestJS = `i18n-test`.js

// rapture-cli-test
lazy val `cli-test` = crossProject.dependsOn(cli, test)
  .settings(moduleName := "rapture-cli-test")
  .settings(raptureSettings:_*)
 
lazy val cliTestJVM = `cli-test`.jvm
lazy val cliTestJS = `cli-test`.js

// rapture-json-test
lazy val `json-test` = crossProject.dependsOn(`json-jawn`, `json-lift`, `json-spray`, `json-argonaut`, `json-jackson`, `json-play`, `json-json4s`, `json-circe`, test)
  .settings(moduleName := "rapture-json-test")
  .settings(raptureSettings:_*)
 
lazy val jsonTestJVM = `json-test`.jvm
lazy val jsonTestJS = `json-test`.js

// rapture-xml-test
lazy val `xml-test` = crossProject.dependsOn(`xml-stdlib`, test)
  .settings(moduleName := "rapture-xml-test")
  .settings(raptureSettings:_*)
 
lazy val xmlTestJVM = `xml-test`.jvm
lazy val xmlTestJS = `xml-test`.js

lazy val publishSettings = Seq(
  homepage := Some(url("http://rapture.io/")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  autoAPIMappings := true,
  publishMavenStyle := true,
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
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  ),
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value
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
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
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

addCommandAlias("gitSnapshots", ";set version in ThisBuild := git.gitDescribedVersion.value.get + \"-SNAPSHOT\"")

// For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
