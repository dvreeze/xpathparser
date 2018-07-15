
// Building both for JVM and JavaScript runtimes.

// To convince SBT not to publish any root level artifacts, I had a look at how scala-java-time does it.
// See https://github.com/cquiroz/scala-java-time/blob/master/build.sbt as a "template" for this build file.

// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val scalaVer = "2.12.6"

val crossScalaVer = Seq(scalaVer, "2.11.12")

lazy val commonSettings = Seq(
  name         := "xpathparser",
  description  := "XPath parser and XPath AST API",
  organization := "eu.cdevreeze.xpathparser",
  version      := "0.5.0-SNAPSHOT",

  scalaVersion       := scalaVer,
  crossScalaVersions := crossScalaVer,

  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings", "-Xlint", "-target:jvm-1.8"),

  ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) },

  publishArtifact in Test := false,
  publishMavenStyle := true,

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },

  pomExtra := pomData,
  pomIncludeRepository := { _ => false },

  libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0",

  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % "test"
)

lazy val root = project.in(file("."))
  .aggregate(xpathparserJVM, xpathparserJS)
  .settings(commonSettings: _*)
  .settings(
    name                 := "xpathparser",
    // Thanks, scala-java-time, for showing us how to prevent any publishing of root level artifacts:
    // No, SBT, we don't want any artifacts for root. No, not even an empty jar.
    publish              := {},
    publishLocal         := {},
    publishArtifact      := false,
    Keys.`package`       := file(""))

lazy val xpathparser = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(commonSettings: _*)
  .jvmSettings(
    mimaPreviousArtifacts := Set("eu.cdevreeze.xpathparser" %%% "xpathparser" % "0.4.0")
  )
  .jsSettings(
    // Do we need this jsEnv?
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),

    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6",

    libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.5.3",

    mimaPreviousArtifacts := Set("eu.cdevreeze.xpathparser" %%% "xpathparser" % "0.4.0")
  )

lazy val xpathparserJVM = xpathparser.jvm
lazy val xpathparserJS = xpathparser.js

lazy val pomData =
  <url>https://github.com/dvreeze/xpathparser</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
      <comments>XPathParser is licensed under Apache License, Version 2.0</comments>
    </license>
  </licenses>
  <scm>
    <connection>scm:git:git@github.com:dvreeze/xpathparser.git</connection>
    <url>https://github.com/dvreeze/xpathparser.git</url>
    <developerConnection>scm:git:git@github.com:dvreeze/xpathparser.git</developerConnection>
  </scm>
  <developers>
    <developer>
      <id>dvreeze</id>
      <name>Chris de Vreeze</name>
      <email>chris.de.vreeze@caiway.net</email>
    </developer>
  </developers>
