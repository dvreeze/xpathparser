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

import cats.parse.{Parser => P}
import eu.cdevreeze.xpathparser.ast.XPathExpr
import org.scalatest.funsuite.AnyFunSuite

/**
 * XPath extraction test case.
 *
 * @author Chris de Vreeze
 */
class ExtractXPathTest extends AnyFunSuite:

  // import XPathElemParser.stringConcatExpr
  import XPathParser.xpathExpr

  test("testParseRhsStringConcatExpr") {
    // From the Dutch KvK taxonomy.

    val exprString =
      """ $Equity = 
        sum($varArc_BalanceSheetVertical_MsgSeparateSumOfChildrenParentCredit1_ChildrenOfEquityCredit)
         - sum($varArc_BalanceSheetVertical_MsgSeparateSumOfChildrenParentCredit1_ChildrenOfEquityDebit)"""

    val parseResult = XPathParser.xpathExpr.parseAll(exprString)

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

    val parseResult = XPathParser.xpathExpr.parseAll(exprString)

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

  private def assertSuccess(parseResult: Either[P.Error, XPathExpr]): Unit =
    assertResult(true, parseResult) {
      parseResult.isRight
    }
