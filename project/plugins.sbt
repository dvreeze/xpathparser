
// For a list of well-known plugins, see https://www.scala-sbt.org/1.x/docs/Community-Plugins.html.

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")

addSbtPlugin("ch.epfl.scala" % "sbt-scala3-migrate" % "0.5.0")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.2")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.7")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.0")

// See https://github.com/albuch/sbt-dependency-check (the plugin checks potential security vulnerabilities)
// Tasks: dependencyCheck, dependencyCheckAggregate, etc.
addSbtPlugin("net.vonbuchholtz" % "sbt-dependency-check" % "5.1.0")

// See https://github.com/rtimush/sbt-updates
// Tasks: dependencyUpdates, dependencyUpdatesReport
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.3")

// See https://github.com/cb372/sbt-explicit-dependencies (like maven-dependency-plugin:analyze)
// Tasks: undeclaredCompileDependencies, undeclaredCompileDependenciesTest, unusedCompileDependencies etc.

// The sbt-explicit-dependencies plugin does not work well with cross-platform builds, it seems. Is that still true?
addSbtPlugin("com.github.cb372" % "sbt-explicit-dependencies" % "0.2.16")

addSbtPlugin("ch.epfl.scala" % "sbt-missinglink" % "0.3.5")

// See https://github.com/sbt/sbt-duplicates-finder (finds duplicates at level of classes etc.)
// Should detect Saxon-HE and Saxon-EE together on classpath
// Tasks: checkDuplicates, checkDuplicatesTest
addSbtPlugin("com.github.sbt" % "sbt-duplicates-finder" % "1.1.0")

addSbtPlugin("io.github.cquiroz" % "sbt-tzdb" % "4.2.0")

libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"

