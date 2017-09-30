
// Run amm in scripts folder
// In amm session, use command "import $exec.eu.cdevreeze.xpathparser.scripts.ExtractTestXPaths"

// This script expects an input directory with the unzipped QT3_1_0 directory, after downloading
// it from https://dev.w3.org/2011/QT3-test-suite/.

// It creates an output property file with test XPath expressions that must be successfully parsed (at
// least). The keys are the test case names.

// Taking yaidom version 1.6.2

import $ivy.`eu.cdevreeze.yaidom::yaidom:1.6.2`

// Imports that (must) remain available after this initialization script

import java.net.URI
import java.io._
import java.util.Properties

import scala.collection.immutable

import eu.cdevreeze.yaidom.core._
import eu.cdevreeze.yaidom._

println("Usage: createTestXPathPropertyFile(inputDir, outputFile)")

val ns = "http://www.w3.org/2010/09/qt-fots-catalog"

def accumulateTestXPaths(doc: simple.Document, props: Properties): Unit = {
  require(doc.documentElement.localName == "test-set")
  
  val testcaseElems = doc.documentElement.filterChildElems(_.localName == "test-case")
  
  val dependencyElemOption = doc.documentElement.findChildElem(_.localName == "dependency")
  
  val skipTests = dependencyElemOption.exists(!_.attribute(EName("value")).contains("XP30"))
  
  if (!skipTests) {
    val filteredTestcaseElems =
      testcaseElems filter { elm =>
        elm.findChildElem(_.localName == "dependency").forall(_.attribute(EName("value")).contains("XP30")) &&
          elm.findElem(_.localName == "error").isEmpty
      }
      
    filteredTestcaseElems foreach { elm =>
      props.setProperty(elm.attribute(EName("name")), elm.getChildElem(_.localName == "test").text)
    }
  }
}

def createTestXPathPropertyFile(inputDir: File, outputFile: File): Unit = {
  require(inputDir.isDirectory, s"Not a directory: $inputDir")
  
  val catalogFile = new File(inputDir, "catalog.xml")
  require(catalogFile.isFile, s"Not a catalog file: $catalogFile")
  
  val docParser = parse.DocumentParserUsingSax.newInstance()
  
  val catalogDoc = docParser.parse(catalogFile)
  
  require(
    catalogDoc.documentElement.resolvedName == EName(ns, "catalog"),
    s"Not a catalog with the expected root element: $catalogFile")
    
  val testElems =
    catalogDoc.documentElement.filterChildElems(e => e.resolvedName == EName(ns, "test-set") && e.attribute(EName("name")).startsWith("prod-"))
    
  val testFiles = testElems.map(e => new File(inputDir, e.attribute(EName("file"))))
    
  val props = new Properties()
  
  val testDocs = testFiles.map(f => docParser.parse(f))
  
  testDocs.foreach(doc => accumulateTestXPaths(doc, props))
  
  props.storeToXML(new FileOutputStream(outputFile), "Test XPath expressions that must be successfully parsed")
}
