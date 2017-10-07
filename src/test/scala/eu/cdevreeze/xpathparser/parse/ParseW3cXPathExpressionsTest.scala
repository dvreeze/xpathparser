/*
 * Copyright 2011-2017 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.xpathparser.parse

import scala.collection.JavaConverters.propertiesAsScalaMapConverter

import org.scalatest.FunSuite

/**
 * XPath parsing test case, taking test data from the W3C test suite https://dev.w3.org/2011/QT3-test-suite.
 *
 * @author Chris de Vreeze
 */
class ParseW3cXPathExpressionsTest extends FunSuite {

  import XPathParser.xpathExpr

  // See for example http://blog.echo.sh/2013/05/12/dynamically-creating-tests-with-scalatest.html.

  private val testInputs: Map[String, String] = {
    val props = new java.util.Properties
    props.loadFromXML(classOf[ParseW3cXPathExpressionsTest].getResourceAsStream("/testXPaths.xml"))

    props.asScala.toMap.filterKeys(_.indexOf("Comment") < 0)
  }

  testInputs foreach {
    case (name, exprString) =>
      test(name) {
        // Do not forget to trim the expression first!

        val parseResult = xpathExpr.parse(exprString.trim)

        assertSuccess(parseResult)
      }
  }

  private def assertSuccess(parseResult: fastparse.all.Parsed[_]): Unit = {
    assertResult(true) {
      parseResult.fold(
        (parser, pos, extra) => false,
        (expr, pos) => true)
    }
  }
}
