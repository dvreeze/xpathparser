
// Building both for JVM and JavaScript runtimes.

// To convince SBT not to publish any root level artifacts, I had a look at how scala-java-time does it.
// See https://github.com/cquiroz/scala-java-time/blob/master/build.sbt as a "template" for this build file.

// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val scalaVer = "3.0.0"
val crossScalaVer = Seq(scalaVer, "2.13.6")

ThisBuild / description  := "XPath parser and XPath AST API"
ThisBuild / organization := "eu.cdevreeze.xpathparser"
ThisBuild / version      := "0.8.0-SNAPSHOT"

ThisBuild / scalaVersion       := scalaVer
ThisBuild / crossScalaVersions := crossScalaVer

ThisBuild / scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
  case (Some((3, _))) =>
    Seq("-unchecked", "-source:3.0-migration")
  case _ =>
    Seq("-Wconf:cat=unused-imports:w,cat=unchecked:w,cat=deprecation:w,cat=feature:w,cat=lint:w", "-Ytasty-reader", "-Xsource:3")
})

ThisBuild / Test / publishArtifact := false
ThisBuild / publishMavenStyle := true

ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  }

ThisBuild / pomExtra := pomData
ThisBuild / pomIncludeRepository := { _ => false }

val catsVersion = "2.6.1"

// This is what I wanted to do, but that caused ScalaJS linker errors. Hence the repeated dependencies below.
// ThisBuild / libraryDependencies += "org.typelevel" %%% "cats-core" % catsVersion
// ThisBuild / libraryDependencies += "org.typelevel" %%% "cats-parse" % "0.3.4"

ThisBuild / libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.9" % Test

lazy val root = project.in(file("."))
  .aggregate(xpathparserJVM, xpathparserJS)
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
  .jvmSettings(
    libraryDependencies += "org.typelevel" %%% "cats-core" % catsVersion,

    libraryDependencies += "org.typelevel" %%% "cats-parse" % "0.3.4",

    mimaPreviousArtifacts := Set("eu.cdevreeze.xpathparser" %%% "xpathparser" % "0.6.1")
  )
  .jsSettings(
    // Do we need this jsEnv?
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),

    scalaJSUseMainModuleInitializer := false,

    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case (Some((3, _))) =>
        Seq(
          // Hopefully for3Use2_13 soon not needed anymore
          "org.scala-js" % "scalajs-dom_sjs1_2.13" % "1.1.0",
        )
      case _ =>
        Seq(
          "org.scala-js" % "scalajs-dom_sjs1_2.13" % "1.1.0",
        )
    }),

    libraryDependencies += "org.typelevel" %%% "cats-core" % catsVersion,

    libraryDependencies += "org.typelevel" %%% "cats-parse" % "0.3.4",

    libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.6.6",

    mimaPreviousArtifacts := Set("eu.cdevreeze.xpathparser" %%% "xpathparser" % "0.6.1")
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
