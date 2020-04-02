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

package eu.cdevreeze.xpathparser.util

import eu.cdevreeze.xpathparser.ast.XPathExpr
import org.scalatest.funsuite.AnyFunSuite

/**
 * EQNameUtil test case.
 *
 * @author Chris de Vreeze
 */
class EQNameUtilTest extends AnyFunSuite {

  import fastparse._

  import eu.cdevreeze.xpathparser.parse.XPathParser.xpathExpr

  test("testPrefixesInSlash") {
    val exprString = "/"

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    assertUsedPrefixes(parseResult.get.value, Set())
  }

  test("testPrefixesInSimplePathExpr") {
    val exprString = "/p:a//p:b/p:c//p:d/p:e"

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    assertUsedPrefixes(parseResult.get.value, Set("p"))
  }

  test("testPrefixesInIfExprWithFunctionCalls") {
    // From the NL taxonomy (NT12)

    val exprString =
      "if(xff:has-fallback-value(xs:QName('varArc_BalanceSheetVertical_MsgPrecondValueConceptAndNoExistenceConcept1_ResultForTheYear'))) " +
        "then true() else not(count($varArc_BalanceSheetVertical_MsgPrecondValueConceptAndNoExistenceConcept1_ResultForTheYear) ge 1)"

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    assertUsedPrefixes(parseResult.get.value, Set("xff", "xs"))
  }

  test("testPrefixesInSummation") {
    // From the NL taxonomy (NT12)

    val exprString =
      "$varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_SumOfMembers =  " +
        "sum($varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_ChildrenMember) "

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    assertUsedPrefixes(parseResult.get.value, Set())
  }

  test("testPrefixesInIfExprWithFunctionCallsAndStringLiterals") {
    // From the NL taxonomy (NT12)

    val exprString =
      "xfi:fact-has-explicit-dimension-value($varArc_DocumentInformation_MsgPrecondExistenceMemberAspect3_AllItems," +
        "xs:QName('venj-bw2-dim:FinancialStatementsTypeAxis'),xs:QName('venj-bw2-dm:SeparateMember'))"

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    assertUsedPrefixes(parseResult.get.value, Set("xfi", "xs"))

    assertUsedPrefixesIncludingFromXsQName(parseResult.get.value, Set("xfi", "xs", "venj-bw2-dim", "venj-bw2-dm"))
  }

  test("testPrefixesInLetExpr") {
    // Example from the XPath 3.0 spec

    val exprString =
      """let $f := function($a) { starts-with($a, "E") }
        return local:filter(("Ethel", "Enid", "Gertrude"), $f)"""

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    assertUsedPrefixes(parseResult.get.value, Set("local"))
  }

  test("testPrefixesInNonTrivialForExpr") {
    // Example from https://github.com/Saxonica/XT-Speedo (adapted)

    val exprString = """for $p:w in //fn:text()/fn:tokenize(., '\W+')[.!=''] return fn:lower-case($p:w)"""

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    assertUsedPrefixes(parseResult.get.value, Set("p", "fn"))
  }

  test("testPrefixesInLetExprWithFreeVars") {
    // Example from the XPath 3.0 spec, adapted to contain some free variables (adapted)

    val exprString =
      """let $p1:f := function($p2:a) { starts-with($p2:a, $p3:b) }
        return local:filter(("Ethel", "Enid", "Gertrude", $p4:c, $Q{}d, $Q{http://www.example.org/}e, $p2:a), $p1:f)"""

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    assertUsedPrefixes(parseResult.get.value, Set("p1", "p2", "p3", "p4", "local"))
  }

  test("testPrefixesInExprWithBindingsDependingOnOtherBindings") {
    // From the NL taxonomy (NT12)

    val exprString =
      """fn:not((for
                $n in (1 to (fn:count($varArc_ELRName_PrtFactsNECovA_Set01)-1)),
                $m in (($n+1) to (fn:count($varArc_ELRName_PrtFactsNECovA_Set01)))
              return (($varArc_ELRName_PrtFactsNECovA_Set01[$n] = $varArc_ELRName_PrtFactsNECovA_Set01[$m]))) = fn:true())""".trim

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    assertUsedPrefixes(parseResult.get.value, Set("fn"))
  }

  test("testPrefixesInCastExpr") {
    val exprString = "xs:NCName('entity') cast as xs:ENTITY"

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    assertUsedPrefixes(parseResult.get.value, Set("xs"))
  }

  test("testPrefixesInNamedFunctionRef") {
    val exprString = "exists(fn:minutes-from-duration#1)"

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    assertUsedPrefixes(parseResult.get.value, Set("fn"))
  }

  test("testPrefixesInSimpleNameTest") {
    val exprString = "let $var := /works return fn:count($var/child::p:employee)"

    val parseResult = parse(exprString, xpathExpr(_))

    assertSuccess(parseResult)

    assertUsedPrefixes(parseResult.get.value, Set("fn", "p"))
  }

  private def assertUsedPrefixes(
    expr: XPathExpr,
    expectedPrefixes: Set[String]): Unit = {

    val prefixes = EQNameUtil.findUsedPrefixes(expr)

    assertResult(expectedPrefixes) {
      prefixes
    }
  }

  private def assertUsedPrefixesIncludingFromXsQName(
    expr: XPathExpr,
    expectedPrefixes: Set[String]): Unit = {

    val prefixes = EQNameUtil.findUsedPrefixes(expr, EQNameUtil.eqnameProducerFromXsQName)

    assertResult(expectedPrefixes) {
      prefixes
    }
  }

  private def assertSuccess(parseResult: Parsed[_]): Unit = {
    assertResult(true) {
      parseResult.fold(
        (parser, pos, extra) => false,
        (expr, pos) => true)
    }
  }
}
