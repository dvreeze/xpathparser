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

import scala.reflect.classTag

import org.scalatest.FunSuite

import eu.cdevreeze.xpathparser.ast.EQName
import eu.cdevreeze.xpathparser.ast.ForExpr
import eu.cdevreeze.xpathparser.ast.InlineFunctionExpr
import eu.cdevreeze.xpathparser.ast.LetExpr
import eu.cdevreeze.xpathparser.ast.XPathElem

/**
 * XPath variable binding test case.
 *
 * Some sources for test XPath expressions were:
 * <ul>
 * <li>https://github.com/Saxonica/XT-Speedo</li>
 * <li>https://en.wikibooks.org/wiki/XQuery/XPath</li>
 * <li>https://www.w3.org/TR/xpath-30</li>
 * <li>http://www.nltaxonomie.nl/nt12/kvk/</li>
 * </ul>
 *
 * @author Chris de Vreeze
 */
class VariableBindingTest extends FunSuite {

  import fastparse.all.Parsed

  import eu.cdevreeze.xpathparser.parse.XPathParser.xpathExpr

  test("testBindingsInSlash") {
    val exprString = "/"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertFreeVariableNames(parseResult.get.value, Set())
    assertBoundVariableNames(parseResult.get.value, Set())
  }

  test("testBindingsInSimplePathExpr") {
    val exprString = "/p:a//p:b/p:c//p:d/p:e"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertFreeVariableNames(parseResult.get.value, Set())
    assertBoundVariableNames(parseResult.get.value, Set())
  }

  test("testBindingsInIfExprWithFunctionCalls") {
    // From the NL taxonomy (NT12)

    val exprString =
      "if(xff:has-fallback-value(xs:QName('varArc_BalanceSheetVertical_MsgPrecondValueConceptAndNoExistenceConcept1_ResultForTheYear'))) " +
        "then true() else not(count($varArc_BalanceSheetVertical_MsgPrecondValueConceptAndNoExistenceConcept1_ResultForTheYear) ge 1)"

    val parseResult = xpathExpr.parse(exprString)

    assertFreeVariableNames(parseResult.get.value, Set(
      EQName.QName("varArc_BalanceSheetVertical_MsgPrecondValueConceptAndNoExistenceConcept1_ResultForTheYear")))
    assertBoundVariableNames(parseResult.get.value, Set())
  }

  test("testBindingsInSummation") {
    // From the NL taxonomy (NT12)

    val exprString =
      "$varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_SumOfMembers =  " +
        "sum($varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_ChildrenMember) "

    val parseResult = xpathExpr.parse(exprString)

    assertFreeVariableNames(parseResult.get.value, Set(
      EQName.QName("varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_SumOfMembers"),
      EQName.QName("varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_ChildrenMember")))
    assertBoundVariableNames(parseResult.get.value, Set())
  }

  test("testBindingsInIfExprWithFunctionCallsAndStringLiterals") {
    // From the NL taxonomy (NT12)

    val exprString =
      "xfi:fact-has-explicit-dimension-value($varArc_DocumentInformation_MsgPrecondExistenceMemberAspect3_AllItems," +
        "xs:QName('venj-bw2-dim:FinancialStatementsTypeAxis'),xs:QName('venj-bw2-dm:SeparateMember'))"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertFreeVariableNames(parseResult.get.value, Set(
      EQName.QName("varArc_DocumentInformation_MsgPrecondExistenceMemberAspect3_AllItems")))
    assertBoundVariableNames(parseResult.get.value, Set())
  }

  test("testLetExpr") {
    // Example from the XPath 3.0 spec

    val exprString =
      """let $f := function($a) { starts-with($a, "E") }
        return local:filter(("Ethel", "Enid", "Gertrude"), $f)"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertFreeVariableNames(parseResult.get.value, Set())
    assertBoundVariableNames(parseResult.get.value, Set(EQName.QName("a"), EQName.QName("f")))

    val funcBody =
      parseResult.get.value.findFirstElemOfType(classTag[InlineFunctionExpr]).get.body

    assertFreeVariableNames(funcBody, Set(EQName.QName("a")))
    assertBoundVariableNames(funcBody, Set())

    assertFreeVariableNames(funcBody, Set(), Set(EQName.QName("a")))
    assertBoundVariableNames(funcBody, Set(EQName.QName("a")), Set(EQName.QName("a")))

    val returnExpr = parseResult.get.value.findFirstElemOrSelfOfType(classTag[LetExpr]).get.returnExpr

    assertFreeVariableNames(returnExpr, Set(EQName.QName("f")))
    assertBoundVariableNames(returnExpr, Set())

    assertFreeVariableNames(returnExpr, Set(), Set(EQName.QName("f")))
    assertBoundVariableNames(returnExpr, Set(EQName.QName("f")), Set(EQName.QName("f")))
  }

  test("testNonTrivialForExpr") {
    // Example from https://github.com/Saxonica/XT-Speedo

    val exprString = """for $w in //text()/tokenize(., '\W+')[.!=''] return lower-case($w)"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertFreeVariableNames(parseResult.get.value, Set())
    assertBoundVariableNames(parseResult.get.value, Set(EQName.QName("w")))

    val returnExpr = parseResult.get.value.findFirstElemOrSelfOfType(classTag[ForExpr]).get.returnExpr

    assertFreeVariableNames(returnExpr, Set(EQName.QName("w")))
    assertBoundVariableNames(returnExpr, Set())

    assertFreeVariableNames(returnExpr, Set(), Set(EQName.QName("w")))
    assertBoundVariableNames(returnExpr, Set(EQName.QName("w")), Set(EQName.QName("w")))
  }

  private def assertFreeVariableNames(
    elem: XPathElem,
    expectedVarNames: Set[EQName],
    inheritedIntroducedVarNames: Set[EQName] = Set()): Unit = {

    val freeVars = VariableBindingUtil.findAllFreeVariables(elem, inheritedIntroducedVarNames)

    assertResult(expectedVarNames) {
      freeVars.map(_.varName).toSet
    }
  }

  private def assertBoundVariableNames(
    elem: XPathElem,
    expectedVarNames: Set[EQName],
    inheritedIntroducedVarNames: Set[EQName] = Set()): Unit = {

    val boundVars = VariableBindingUtil.findAllBoundVariables(elem, inheritedIntroducedVarNames)

    assertResult(expectedVarNames) {
      boundVars.map(_.varName).toSet
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
