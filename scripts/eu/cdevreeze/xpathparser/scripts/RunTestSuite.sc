
// Run amm in scripts folder
// In amm session, use command "import $exec.eu.cdevreeze.xpathparser.scripts.RunTestSuite"

// This script expects an input property file with test XPaths, like src/test/resources/testXPaths.
// It then runs the tests, and shows which ones fail.

// Taking xpathparser version 0.8.0

import $ivy.`eu.cdevreeze.xpathparser::xpathparser:0.8.0`

// Imports that (must) remain available after this initialization script

import java.net.URI
import java.io._
import java.util.Properties

import scala.jdk.CollectionConverters._
import scala.reflect.classTag
import scala.util._

import eu.cdevreeze.xpathparser.ast._
import eu.cdevreeze.xpathparser.parse.XPathParser._

println("Usage: runTests(testInputFile)")

def runTests(testInputFile: File): Unit = {
  val props = new Properties
  props.loadFromXML(new FileInputStream(testInputFile))
  
  val testMapping: Map[String, String] = props.asScala.toMap
  
  val rawParsedXPathMapping: Map[String, Try[Either[_, XPathExpr]]] = testMapping.view.mapValues { exprString =>
    Try(xpathExpr.parseAll(exprString))
  }.toMap
  
  val parseExceptionMapping: Map[String, Failure[_]] = rawParsedXPathMapping.collect {
    case (testName, t @ Failure(_)) => (testName -> t)
  }
  
  val parsedXPathMapping: Map[String, Success[Either[_, XPathExpr]]] = rawParsedXPathMapping.collect {
    case (testName, t @ Success(_)) => (testName -> t)
  }
  
  val parseFailureMapping: Map[String, Left[_, XPathExpr]] = parsedXPathMapping.collect {
    case (testName, t @ Success(e @ Left(_))) => (testName -> e)
  }

  val parseSuccessMapping: Map[String, XPathExpr] = parsedXPathMapping.collect {
    case (testName, t @ Success(Right(v))) => (testName -> v)
  }
  
  println()
  println("Parse successes:")
  println()

  parseSuccessMapping.toSeq.sortBy(_._1).foreach {
    case (testName, parsedXPath) =>
      println(s"Success: $testName. Number of elements: ${parsedXPath.findAllElemsOrSelf.size}")
  }

  println()
  println("Parse failures:")
  println()

  parseFailureMapping.toSeq.sortBy(_._1).foreach {
    case (testName, parseResult) => println(s"Failure: $testName. Result: $parseResult")
  }
  
  println()
  println("Fatal errors:")
  println()

  parseExceptionMapping.toSeq.sortBy(_._1).foreach {
    case (testName, exceptionResult) => println(s"Fatal error: $testName. Error: $exceptionResult")
  }

  println()
  println(s"Number of fatal parse exceptions: ${parseExceptionMapping.size}")
  println(s"Number of parsed expressions: ${parsedXPathMapping.size}")

  println(s"Number of parse successes: ${parseSuccessMapping.size}")
  println(s"Number of parse failures (non-fatal): ${parseFailureMapping.size}")
}
