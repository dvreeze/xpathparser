
// Run amm in scripts folder
// In amm session, use command "import $exec.eu.cdevreeze.xpathparser.scripts.RunTestSuite"

// This script expects an input property file with test XPaths, like src/test/resources/testXPaths.
// It then runs the tests, and shows which ones fail.

// Taking xpathparser version 0.3.0

import $ivy.`eu.cdevreeze.xpathparser::xpathparser:0.3.0`

// Imports that (must) remain available after this initialization script

import java.net.URI
import java.io._
import java.util.Properties

import scala.collection.immutable
import scala.collection.JavaConverters._
import scala.reflect.classTag
import scala.util._

import eu.cdevreeze.xpathparser.ast._
import eu.cdevreeze.xpathparser.parse.XPathParser.xpathExpr
import fastparse.all._

println("Usage: runTests(testInputFile)")

def runTests(testInputFile: File): Unit = {
  val props = new Properties
  props.loadFromXML(new FileInputStream(testInputFile))
  
  val testMapping: Map[String, String] = props.asScala.toMap
  
  val rawParsedXPathMapping = testMapping mapValues { exprString =>
    Try(xpathExpr.parse(exprString.trim))
  }
  
  val parseExceptionMapping = rawParsedXPathMapping collect { 
    case (testName, t @ Failure(_)) => (testName -> t)
  }
  
  val parsedXPathMapping = rawParsedXPathMapping collect {
    case (testName, t @ Success(_)) => (testName -> t.get)
  }
  
  val parseFailureMapping = parsedXPathMapping collect {
    case (testName, t: Parsed.Failure) => (testName -> t)
  }

  val parseSuccessMapping: Map[String, XPathExpr] = parsedXPathMapping collect {
    case (testName, t: Parsed.Success[_]) => (testName -> t.get.value.asInstanceOf[XPathExpr])
  }
  
  println()
  println("Parse successes:")
  println()

  parseSuccessMapping.toSeq.sortBy(_._1) foreach {
    case (testName, parsedXPath) =>
      println(s"Success: $testName. Number of elements: ${parsedXPath.findAllElemsOrSelf.size}")
  }

  println()
  println("Parse failures:")
  println()

  parseFailureMapping.toSeq.sortBy(_._1) foreach {
    case (testName, parseResult) => println(s"Failure: $testName. Result: $parseResult")
  }
  
  println()
  println("Fatal errors:")
  println()

  parseExceptionMapping.toSeq.sortBy(_._1) foreach {
    case (testName, exceptionResult) => println(s"Fatal error: $testName. Error: $exceptionResult")
  }

  println()
  println(s"Number of fatal parse exceptions: ${parseExceptionMapping.size}")
  println(s"Number of parsed expressions: ${parsedXPathMapping.size}")

  println(s"Number of parse successes: ${parseSuccessMapping.size}")
  println(s"Number of parse failures (non-fatal): ${parseFailureMapping.size}")
}
