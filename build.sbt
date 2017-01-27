import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import ReleaseTransformations._

enablePlugins(GitBranchPrompt)

lazy val buildSettings = Seq(
  organization := "com.propensive",
  scalaVersion := "2.12.1",
  crossScalaVersions := Seq("2.12.1", "2.11.8", "2.10.6")
)

lazy val commonSettings = Seq(
//  scalafmtConfig in ThisBuild := Some(file(".scalafmt")),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-language:existentials"
  /*  "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-Yinline-warnings",
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture" */
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, majorVersion)) if majorVersion >= 11 => Seq("-Ywarn-unused-import")
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
  .aggregate(raptureJVM, raptureJS, raptureExtrasJVM, raptureExtrasJS)
  .dependsOn(raptureJVM, raptureJS, raptureExtrasJVM, raptureExtrasJS)

lazy val raptureJVM = project.in(file(".raptureJVM"))
  .settings(moduleName := "rapture")
  .settings(raptureSettings)
  .aggregate(baseJVM, coreJVM, timeJVM, uriJVM, codecJVM, cryptoJVM, csvJVM, ioJVM, fsJVM, netJVM, httpJVM, mimeJVM, cliJVM, mailJVM, logJVM, i18nJVM, googleTranslateJVM, textJVM, latexJVM, testJVM, dataJVM, xmlJVM, jsJVM, cssJVM, currencyJVM, jsonJVM, htmlJVM, domJVM, coreScalazJVM, httpJsonJVM)
  .dependsOn(baseJVM, coreJVM, timeJVM, uriJVM, codecJVM, cryptoJVM, csvJVM, ioJVM, fsJVM, netJVM, httpJVM, mimeJVM, cliJVM, mailJVM, logJVM, i18nJVM, googleTranslateJVM, textJVM, latexJVM, testJVM, dataJVM, xmlJVM, jsJVM, cssJVM, currencyJVM, jsonJVM, htmlJVM, domJVM, coreScalazJVM, httpJsonJVM)
  
lazy val raptureJS = project.in(file(".raptureJS"))
  .settings(moduleName := "rapture")
  .settings(raptureSettings)
  .aggregate(baseJS, coreJS, timeJS, uriJS, codecJS, cryptoJS, csvJS, ioJS, fsJS, netJS, httpJS, mimeJS, cliJS, mailJS, logJS, i18nJS, googleTranslateJS, textJS, latexJS, testJS, dataJS, jsonJS, htmlJS, domJS, coreScalazJS, httpJsonJS, xmlJS, jsJS, cssJS, currencyJS)
  .dependsOn(baseJS, coreJS, timeJS, uriJS, codecJS, cryptoJS, csvJS, ioJS, fsJS, netJS, httpJS, mimeJS, cliJS, mailJS, logJS, i18nJS, googleTranslateJS, textJS, latexJS, testJS, dataJS, jsonJS, htmlJS, domJS, coreScalazJS, httpJsonJS, xmlJS, jsJS, cssJS, currencyJS)
  .enablePlugins(ScalaJSPlugin)

lazy val raptureExtras = crossProject
  .aggregate(`core-test`, `http-jetty`, `json-circe`, `xml-stdlib`, `json-jawn`, `json-play`, `json-json4s`, `json-spray`, `json-argonaut`, `json-jackson`, `json-test`, `xml-test`, `json-lift`)
  .dependsOn(`core-test`, `http-jetty`, `json-circe`, `xml-stdlib`, `json-jawn`, `json-play`, `json-json4s`, `json-spray`, `json-argonaut`, `json-jackson`, `json-test`, `xml-test`, `json-lift`)
  .settings(moduleName := "rapture-extras")
  .settings(raptureSettings:_*)
  .settings(crossVersionSharedSources():_*)

lazy val raptureExtrasJVM = raptureExtras.jvm
lazy val raptureExtrasJS = raptureExtras.js

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

// rapture-core-test
lazy val `core-test` = crossProject.dependsOn(test, `core-scalaz`)
  .settings(moduleName := "rapture-core-test")
  .settings(raptureSettings:_*)

lazy val coreTestJVM = `core-test`.jvm
lazy val coreTestJS = `core-test`.js

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
  .settings(libraryDependencies += "org.w3c.css" % "sac" % "1.3")
  .settings(libraryDependencies += "net.sourceforge.cssparser" % "cssparser" % "0.9.20")
 
lazy val httpJVM = http.jvm
lazy val httpJS = http.js

// rapture-http-json
lazy val `http-json` = crossProject.dependsOn(http, json)
  .settings(moduleName := "rapture-http-json")
  .settings(raptureSettings:_*)
 
lazy val httpJsonJVM = `http-json`.jvm
lazy val httpJsonJS = `http-json`.js

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

// rapture-mail
lazy val mail = crossProject.dependsOn(io, html, net)
  .settings(moduleName := "rapture-mail")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "javax.mail" % "mail" % "1.4")
 
lazy val mailJVM = mail.jvm
lazy val mailJS = mail.js

// rapture-log
lazy val log = crossProject.dependsOn(io)
  .settings(moduleName := "rapture-log")
  .settings(raptureSettings:_*)
 
lazy val logJVM = log.jvm
lazy val logJS = log.js

// rapture-i18n
lazy val i18n = crossProject.dependsOn(core, test)
  .settings(moduleName := "rapture-i18n")
  .settings(raptureSettings:_*)
 
lazy val i18nJVM = i18n.jvm
lazy val i18nJS = i18n.js

// rapture-google-translate
lazy val `google-translate` = crossProject.dependsOn(core, net, `json-jawn`, i18n)
  .settings(moduleName := "rapture-google-translate")
  .settings(raptureSettings:_*)
 
lazy val googleTranslateJVM = `google-translate`.jvm
lazy val googleTranslateJS = `google-translate`.js

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
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1")
 
lazy val testJVM = test.jvm
lazy val testJS = test.js

// rapture-dom
lazy val dom = crossProject.dependsOn(core)
  .settings(moduleName := "rapture-dom")
  .settings(raptureSettings:_*)
 
lazy val domJVM = dom.jvm
lazy val domJS = dom.js

// rapture-html
lazy val html = crossProject.dependsOn(net, mime, dom, test, js, css)
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

// rapture-js
lazy val js = crossProject.dependsOn(data)
  .settings(moduleName := "rapture-js")
  .settings(raptureSettings:_*)
 
lazy val jsJVM = js.jvm
lazy val jsJS = js.js

// rapture-css
lazy val css = crossProject.dependsOn(data, dom)
  .settings(moduleName := "rapture-css")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "net.sourceforge.cssparser" % "cssparser" % "0.9.20")
 
lazy val cssJVM = css.jvm
lazy val cssJS = css.js

// rapture-currency
lazy val currency = crossProject.dependsOn(data)
  .settings(moduleName := "rapture-currency")
  .settings(raptureSettings:_*)
 
lazy val currencyJVM = currency.jvm
lazy val currencyJS = currency.js

// rapture-json
lazy val json = crossProject.dependsOn(data)
  .settings(moduleName := "rapture-json")
  .settings(raptureSettings:_*)
 
lazy val jsonJVM = json.jvm
lazy val jsonJS = json.js

// rapture-json-circe
val circeVersion = "0.7.0"
lazy val `json-circe` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-circe")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "io.circe" %% "circe-core" % "0.7.0")
  .settings(libraryDependencies += "io.circe" %% "circe-jawn" % "0.7.0")
 
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
  .settings(libraryDependencies += "org.spire-math" %% "jawn-parser" % "0.10.4")
  .settings(libraryDependencies += "org.spire-math" %% "jawn-ast" % "0.10.4")
 
lazy val jsonJawnJVM = `json-jawn`.jvm
lazy val jsonJawnJS = `json-jawn`.js


lazy val playJsonDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 10)) => "com.typesafe.play" %% "play-json" % "2.4.6"
    case Some((2, 11)) => "com.typesafe.play" %% "play-json" % "2.5.3"
    case Some((2, 12)) => "com.typesafe.play" %% "play-json" % "2.6.0-M1"
  })
)

// rapture-json-play
lazy val `json-play` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-play")
  .settings(raptureSettings: _*)
  .settings(playJsonDependencies: _*)
 
lazy val jsonPlayJVM = `json-play`.jvm
lazy val jsonPlayJS = `json-play`.js

// rapture-json-json4s
lazy val `json-json4s` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-json4s")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.0")
 
lazy val jsonJson4sJVM = `json-json4s`.jvm
lazy val jsonJson4sJS = `json-json4s`.js

// rapture-json-spray
lazy val `json-spray` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-spray")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "io.spray" %% "spray-json" % "1.3.3")
 
lazy val jsonSprayJVM = `json-spray`.jvm
lazy val jsonSprayJS = `json-spray`.js

// rapture-json-argonaut
lazy val `json-argonaut` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-argonaut")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "io.argonaut" %% "argonaut" % "6.2-RC1")
 
lazy val jsonArgonautJVM = `json-argonaut`.jvm
lazy val jsonArgonautJS = `json-argonaut`.js

// rapture-json-jackson
lazy val `json-jackson` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-jackson")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.7.2")
 
lazy val jsonJacksonJVM = `json-jackson`.jvm
lazy val jsonJacksonJS = `json-jackson`.js

// rapture-core-scalaz
lazy val `core-scalaz` = crossProject.dependsOn(core)
  .settings(moduleName := "rapture-core-scalaz")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.8")
  .settings(libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.8")
 
lazy val coreScalazJVM = `core-scalaz`.jvm
lazy val coreScalazJS = `core-scalaz`.js

// rapture-json-test
lazy val `json-test` = crossProject.dependsOn(`json-jawn`, `json-lift`, `json-spray`, `json-argonaut`, `json-jackson`, `json-play`, `json-json4s`, `json-circe`, test)
  .settings(moduleName := "rapture-json-test")
  .settings(raptureSettings:_*)
 
lazy val jsonTestJVM = `json-test`.jvm
lazy val jsonTestJS = `json-test`.js

// rapture-css-test
lazy val `css-test` = crossProject.dependsOn(css, html, test)
  .settings(moduleName := "rapture-css-test")
  .settings(raptureSettings:_*)
 
lazy val cssTestJVM = `css-test`.jvm
lazy val cssTestJS = `css-test`.js

// rapture-xml-test
lazy val `xml-test` = crossProject.dependsOn(`xml-stdlib`, test)
  .settings(moduleName := "rapture-xml-test")
  .settings(raptureSettings:_*)
 
lazy val xmlTestJVM = `xml-test`.jvm
lazy val xmlTestJS = `xml-test`.js

// rapture-json-lift
lazy val `json-lift` = crossProject.dependsOn(json)
  .settings(moduleName := "rapture-json-lift")
  .settings(raptureSettings:_*)
  .settings(libraryDependencies += (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 10)) => "net.liftweb" %% "lift-json" % "2.6.3"
    case Some((2, scalaMajor)) if scalaMajor >= 11 => "net.liftweb" %% "lift-json" % "3.0.1"
  }))

lazy val jsonLiftJVM = `json-lift`.jvm
lazy val jsonLiftJS = `json-lift`.js

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
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
              "org.scalamacros" %% "quasiquotes" % "2.1.0" cross CrossVersion.binary
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
