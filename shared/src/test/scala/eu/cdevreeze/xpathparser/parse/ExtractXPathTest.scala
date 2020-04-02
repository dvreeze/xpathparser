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

import fastparse._
import org.scalatest.funsuite.AnyFunSuite

/**
 * XPath extraction test case.
 *
 * @author Chris de Vreeze
 */
class ExtractXPathTest extends AnyFunSuite {

  import fastparse.Parsed

  // import XPathElemParser.stringConcatExpr
  import XPathParser.xpathExpr

  test("testParseRhsStringConcatExpr") {
    // From the Dutch KvK taxonomy.

    val exprString =
      """ $Equity = 
        sum($varArc_BalanceSheetVertical_MsgSeparateSumOfChildrenParentCredit1_ChildrenOfEquityCredit)
         - sum($varArc_BalanceSheetVertical_MsgSeparateSumOfChildrenParentCredit1_ChildrenOfEquityDebit)"""

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    /*
    val expectedStringConcatExprString =
      exprString.trim.drop("$Equity =".length).trim.ensuring(_.startsWith("sum"))

    assertResult(Some(expectedStringConcatExprString)) {
      ExtractXPathTest.extractOptionalRightHandSideStringConcatExpr(exprString)
    }

    val parseResult2 = parse(expectedStringConcatExprString, stringConcatExpr(_))

    assertSuccess(parseResult2)
    */
  }

  test("testParseLhsStringConcatExpr") {
    // From the Dutch KvK taxonomy, but reversing the var-ref and string-concat-expr.

    val exprString =
      """ sum($varArc_BalanceSheetVertical_MsgSeparateSumOfChildrenParentCredit1_ChildrenOfEquityCredit)
         - sum($varArc_BalanceSheetVertical_MsgSeparateSumOfChildrenParentCredit1_ChildrenOfEquityDebit) = $Equity"""

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    /*
    val expectedStringConcatExprString =
      exprString.trim.dropRight("= $Equity".length).trim.ensuring(_.startsWith("sum"))

    assertResult(Some(expectedStringConcatExprString)) {
      ExtractXPathTest.extractOptionalLeftHandSideStringConcatExpr(exprString)
    }

    val parseResult2 = parse(expectedStringConcatExprString, stringConcatExpr(_))

    assertSuccess(parseResult2)
    */
  }

  private def assertSuccess(parseResult: Parsed[_]): Unit = {
    assertResult(true) {
      parseResult.fold(
        (parser, pos, extra) => false,
        (expr, pos) => true)
    }
  }
}

object ExtractXPathTest {

  import fastparse.NoWhitespace._
  import XPathElemParser._
  import fastparse._

  // Extracting string concatenation expressions as XPath strings from containing XPath strings.
  // It uses some XPathElemParser parsers as lookahead parsers, and emulates the same parsers while extracting
  // string indices of the interesting XPath sub-expressions using the Fastparse Index parser.

  // Note that hasRightHandSideStringConcatExpr and hasLeftHandSideStringConcatExpr are not mutually exclusive!

  def hasRightHandSideStringConcatExpr[_: P]: P[Unit] =
    P(varRef ~ comp ~ stringConcatExpr).map(_ => ())

  def hasLeftHandSideStringConcatExpr[_: P]: P[Unit] =
    P(stringConcatExpr ~ comp ~ varRef).map(_ => ())

  def rightHandSideStringConcatExprIndices[_: P]: P[(Int, Int)] =
    P(Start ~ &(hasRightHandSideStringConcatExpr) ~ varRef ~ comp ~ Index ~ stringConcatExpr ~ Index ~ End) map {
      case (varRef, comp, startIdx, scExpr, endIdx) => (startIdx, endIdx)
    }

  def leftHandSideStringConcatExprIndices[_: P]: P[(Int, Int)] =
    P(Start ~ &(hasLeftHandSideStringConcatExpr) ~ Index ~ stringConcatExpr ~ Index ~ comp ~ varRef ~ End) map {
      case (startIdx, scExpr, endIdx, comp, varRef) => (startIdx, endIdx)
    }

  /*
  def extractOptionalRightHandSideStringConcatExpr(inputString: String): Option[String] = {
    parse(inputString, rightHandSideStringConcatExprIndices(_)) match {
      case Parsed.Success(_, index) =>
        Some(inputString.substring(0, index).trim)
      case Parsed.Failure(_, _, _) =>
        None
    }
  }

  def extractOptionalLeftHandSideStringConcatExpr(inputString: String): Option[String] = {
    parse(inputString, leftHandSideStringConcatExprIndices(_)) match {
      case Parsed.Success(_, index) =>
        Some(inputString.substring(0, index).trim)
      case Parsed.Failure(_, _, _) =>
        None
    }
  }
  */
}
