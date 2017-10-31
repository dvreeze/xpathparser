
name := "xpathparser"

organization := "eu.cdevreeze.xpathparser"

version := "0.3.2-SNAPSHOT"

scalaVersion := "2.12.4"

crossScalaVersions := Seq("2.12.4", "2.11.11")

// See: Toward a safer Scala
// http://downloads.typesafe.com/website/presentations/ScalaDaysSF2015/Toward%20a%20Safer%20Scala%20@%20Scala%20Days%20SF%202015.pdf

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings", "-Xlint")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"


// resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

// addCompilerPlugin("com.artima.supersafe" %% "supersafe" % "1.0.3")

publishMavenStyle := true

publishTo := {
  val vers = version.value

  val nexus = "https://oss.sonatype.org/"

  if (vers.trim.endsWith("SNAPSHOT")) {
    Some("snapshots" at nexus + "content/repositories/snapshots")
  } else {
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  }
}

publishArtifact in Test := false

pomIncludeRepository := { repo => false }

pomExtra := {
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
}
