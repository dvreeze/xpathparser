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

import scala.reflect.classTag

import org.scalatest.FunSuite

import eu.cdevreeze.xpathparser.ast.QNameAsEQName
import eu.cdevreeze.xpathparser.ast.URIQualifiedName
import eu.cdevreeze.xpathparser.ast.XPathExpressions.AbbrevForwardStep
import eu.cdevreeze.xpathparser.ast.XPathExpressions.AdditionOp
import eu.cdevreeze.xpathparser.ast.XPathExpressions.AxisStep
import eu.cdevreeze.xpathparser.ast.XPathExpressions.ContextItemExpr
import eu.cdevreeze.xpathparser.ast.XPathExpressions.ExprSingle
import eu.cdevreeze.xpathparser.ast.XPathExpressions.ForExpr
import eu.cdevreeze.xpathparser.ast.XPathExpressions.ForwardAxisStep
import eu.cdevreeze.xpathparser.ast.XPathExpressions.FunctionCall
import eu.cdevreeze.xpathparser.ast.XPathExpressions.GeneralComp
import eu.cdevreeze.xpathparser.ast.XPathExpressions.IfExpr
import eu.cdevreeze.xpathparser.ast.XPathExpressions.InlineFunctionExpr
import eu.cdevreeze.xpathparser.ast.XPathExpressions.IntegerLiteral
import eu.cdevreeze.xpathparser.ast.XPathExpressions.LetExpr
import eu.cdevreeze.xpathparser.ast.XPathExpressions.Predicate
import eu.cdevreeze.xpathparser.ast.XPathExpressions.SimpleNameTest
import eu.cdevreeze.xpathparser.ast.XPathExpressions.StepExpr
import eu.cdevreeze.xpathparser.ast.XPathExpressions.StringLiteral
import eu.cdevreeze.xpathparser.ast.XPathExpressions.UnaryOp
import eu.cdevreeze.xpathparser.ast.XPathExpressions.ValueComp
import eu.cdevreeze.xpathparser.ast.XPathExpressions.VarRef
import eu.cdevreeze.xpathparser.common.EName

/**
 * XPath parsing test case.
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
class ParseXPathTest extends FunSuite {

  import fastparse.all.Parsed

  import XPathParser.xpathExpr

  test("testParseSlash") {
    val exprString = "/"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)
  }

  test("testParseDoubleSlash") {
    val exprString = "//"

    val parseResult = xpathExpr.parse(exprString)

    assertFailure(parseResult)
  }

  test("testParseSimplePathExpr") {
    val exprString = "/p:a//p:b/p:c//p:d/p:e"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    val simpleNameTests = parseResult.get.value.findAllElemsOfType(classTag[SimpleNameTest])

    assertResult(List(
      QNameAsEQName("p:a"),
      QNameAsEQName("p:b"),
      QNameAsEQName("p:c"),
      QNameAsEQName("p:d"),
      QNameAsEQName("p:e"))) {

      simpleNameTests.map(e => e.name)
    }

    assertResult(5) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[AxisStep]).size
    }
  }

  test("testParseSimplePathExprWithError") {
    val exprString = "/p:a//p:b/p:c//p:d/p:e{"

    val parseResult = xpathExpr.parse(exprString)

    assertFailure(parseResult)
  }

  test("testParseIfExprWithFunctionCalls") {
    // From the NL taxonomy (NT12)

    val exprString =
      "if(xff:has-fallback-value(xs:QName('varArc_BalanceSheetVertical_MsgPrecondValueConceptAndNoExistenceConcept1_ResultForTheYear'))) " +
        "then true() else not(count($varArc_BalanceSheetVertical_MsgPrecondValueConceptAndNoExistenceConcept1_ResultForTheYear) ge 1)"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[IfExpr]).size
    }

    assertResult(5) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[FunctionCall]).size
    }

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[VarRef]).size
    }

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[StringLiteral]).size
    }

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[IntegerLiteral]).size
    }

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[ValueComp.Ge.type]).size
    }

    assertResult(List(QNameAsEQName("varArc_BalanceSheetVertical_MsgPrecondValueConceptAndNoExistenceConcept1_ResultForTheYear"))) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[VarRef]).map(_.varName)
    }
  }

  test("testParseSummation") {
    // From the NL taxonomy (NT12)

    val exprString =
      "$varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_SumOfMembers =  " +
        "sum($varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_ChildrenMember) "

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[GeneralComp.Eq.type]).size
    }

    assertResult(2) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[VarRef]).size
    }

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[FunctionCall]).size
    }

    assertResult(List(
      QNameAsEQName("varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_SumOfMembers"),
      QNameAsEQName("varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_ChildrenMember"))) {

      parseResult.get.value.findAllElemsOrSelfOfType(classTag[VarRef]).map(_.varName)
    }
  }

  test("testParseLargeSummation") {
    // From the NL taxonomy (NT12)

    val exprString =
      "+ sum($varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSevenVariables1_ShareCapitalNumberSharesIssue)" +
        "  + sum($varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSevenVariables1_ShareCapitalNumberSharesPurchase)" +
        " + sum($varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSevenVariables1_ShareCapitalNumberSharesGranting)" +
        " + sum($varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSevenVariables1_ShareCapitalNumberSharesSale)" +
        " + sum($varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSevenVariables1_ShareCapitalNumberSharesWithdrawal)" +
        " + sum($varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSevenVariables1_ShareCapitalNumberSharesDistributionAsDividend)" +
        " + sum($varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSevenVariables1_ShareCapitalNumberSharesOtherMovements)" +
        " =  $varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSevenVariables1_ShareCapitalNumberSharesMovement "

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[GeneralComp.Eq.type]).size
    }

    assertResult(8) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[VarRef]).size
    }

    assertResult(7) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[FunctionCall]).size
    }

    assertResult(true) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[VarRef]).map(_.varName).
        forall(nm => nm.toString.startsWith("varArc_NotesShareCapitalStatementOfChanges"))
    }
  }

  test("testParseIfExprWithFunctionCallsAndStringLiterals") {
    // From the NL taxonomy (NT12)

    val exprString =
      "xfi:fact-has-explicit-dimension-value($varArc_DocumentInformation_MsgPrecondExistenceMemberAspect3_AllItems," +
        "xs:QName('venj-bw2-dim:FinancialStatementsTypeAxis'),xs:QName('venj-bw2-dm:SeparateMember'))"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(3) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[FunctionCall]).size
    }

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[VarRef]).size
    }

    assertResult(2) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[StringLiteral]).size
    }

    assertResult(List(QNameAsEQName("varArc_DocumentInformation_MsgPrecondExistenceMemberAspect3_AllItems"))) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[VarRef]).map(_.varName)
    }
  }

  test("testParseComplexIf") {
    // From the NL taxonomy (NT12)

    val exprString =
      "if(xfi:is-instant-period(xfi:period($varArc_DocumentInformation_MsgContextDatesParamPPEtCE1_AllItems)))then " +
        "((xfi:period-instant(xfi:period($varArc_DocumentInformation_MsgContextDatesParamPPEtCE1_AllItems)) = " +
        "xs:dateTime($FinancialReportingPeriodPrePreviousEndDateParam)+ xs:dayTimeDuration('PT24H'))or " +
        "(xfi:period-instant(xfi:period($varArc_DocumentInformation_MsgContextDatesParamPPEtCE1_AllItems)) = " +
        "xs:dateTime($FinancialReportingPeriodPreviousEndDateParam)+ xs:dayTimeDuration('PT24H'))or " +
        "(xfi:period-instant(xfi:period($varArc_DocumentInformation_MsgContextDatesParamPPEtCE1_AllItems)) = " +
        "xs:dateTime($FinancialReportingPeriodCurrentEndDateParam)+ xs:dayTimeDuration('PT24H')))else " +
        "(if(xfi:is-start-end-period(xfi:period($varArc_DocumentInformation_MsgContextDatesParamPPEtCE1_AllItems)))then " +
        "(((xfi:period-end(xfi:period($varArc_DocumentInformation_MsgContextDatesParamPPEtCE1_AllItems)) = " +
        "(xs:dateTime($FinancialReportingPeriodPreviousEndDateParam)+ xs:dayTimeDuration('PT24H'))) and " +
        "(xfi:period-start(xfi:period($varArc_DocumentInformation_MsgContextDatesParamPPEtCE1_AllItems)) = " +
        "(xs:dateTime($FinancialReportingPeriodPreviousStartDateParam))))or " +
        "((xfi:period-end(xfi:period($varArc_DocumentInformation_MsgContextDatesParamPPEtCE1_AllItems)) = " +
        "(xs:dateTime($FinancialReportingPeriodCurrentEndDateParam)+ xs:dayTimeDuration('PT24H'))) and " +
        "(xfi:period-start(xfi:period($varArc_DocumentInformation_MsgContextDatesParamPPEtCE1_AllItems)) = " +
        "(xs:dateTime($FinancialReportingPeriodCurrentStartDateParam)))))else (false()))"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(2) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[IfExpr]).size
    }

    assertResult(Set(
      QNameAsEQName("xfi:is-instant-period"),
      QNameAsEQName("xfi:period"),
      QNameAsEQName("xfi:period-instant"),
      QNameAsEQName("xs:dateTime"),
      QNameAsEQName("xs:dayTimeDuration"),
      QNameAsEQName("xfi:is-start-end-period"),
      QNameAsEQName("xfi:period-end"),
      QNameAsEQName("xfi:period-start"),
      QNameAsEQName("false"))) {

      parseResult.get.value.findAllElemsOrSelfOfType(classTag[FunctionCall]).map(_.functionName).toSet
    }

    assertResult(16) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[VarRef]).size
    }
  }

  test("testParseExprWithUnaryOp") {
    // From the NL taxonomy (NT12)

    val exprString =
      " $AuditorsFees = - sum($varArc_NotesAuditorsFeesBreakdown_MsgSeparateSumOfChildrenParentDebitDimensionFilter1_ChildrenOfAuditorsFeesCredit)+ " +
        "sum($varArc_NotesAuditorsFeesBreakdown_MsgSeparateSumOfChildrenParentDebitDimensionFilter1_ChildrenOfAuditorsFeesDebit)"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(3) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[VarRef]).size
    }

    assertResult(2) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[FunctionCall]).size
    }

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[UnaryOp.Minus.type]).size
    }

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[AdditionOp.Plus.type]).size
    }
  }

  test("testParseExprWithStringLiteral") {
    // From the NL taxonomy (NT12)

    val exprString =
      "xfi:identifier-scheme(xfi:identifier($varArc_EntityInformation_MsgEqualToIdentifierScheme1_AllItems)) eq 'http://www.kvk.nl/kvk-id'"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOfType(classTag[ValueComp.Eq.type]).size
    }

    assertResult(Set(QNameAsEQName("varArc_EntityInformation_MsgEqualToIdentifierScheme1_AllItems"))) {
      parseResult.get.value.findAllElemsOfType(classTag[VarRef]).map(_.varName).toSet
    }

    assertResult(Set(QNameAsEQName("xfi:identifier"), QNameAsEQName("xfi:identifier-scheme"))) {
      parseResult.get.value.findAllElemsOfType(classTag[FunctionCall]).map(_.functionName).toSet
    }

    assertResult(Set("http://www.kvk.nl/kvk-id")) {
      parseResult.get.value.findAllElemsOfType(classTag[StringLiteral]).map(_.value).toSet
    }
  }

  test("testParseExprWithStringLiteralAndUriQualifiedNames") {
    // From the NL taxonomy (NT12), but adapted

    val XfiNs = "http://www.xbrl.org/2008/function/instance"

    val exprString =
      s"""Q{$XfiNs}identifier-scheme(Q{$XfiNs}identifier($$varArc_EntityInformation_MsgEqualToIdentifierScheme1_AllItems)) eq 'http://www.kvk.nl/kvk-id'"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOfType(classTag[ValueComp.Eq.type]).size
    }

    assertResult(Set(QNameAsEQName("varArc_EntityInformation_MsgEqualToIdentifierScheme1_AllItems"))) {
      parseResult.get.value.findAllElemsOfType(classTag[VarRef]).map(_.varName).toSet
    }

    assertResult(Set(URIQualifiedName(EName(XfiNs, "identifier")), URIQualifiedName(EName(XfiNs, "identifier-scheme")))) {
      parseResult.get.value.findAllElemsOfType(classTag[FunctionCall]).map(_.functionName).toSet
    }

    assertResult(Set("http://www.kvk.nl/kvk-id")) {
      parseResult.get.value.findAllElemsOfType(classTag[StringLiteral]).map(_.value).toSet
    }
  }

  test("testMultipleExprSingles") {
    val exprString = "/p:a/p:b[@xlink:type = 'arc'], if (//p:c) then //p:c else //p:d"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    val topmostExprSingles = parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[ExprSingle])

    assertResult(2) {
      topmostExprSingles.size
    }

    assertResult(Set(QNameAsEQName("p:a"), QNameAsEQName("p:b"), QNameAsEQName("xlink:type"))) {
      topmostExprSingles(0).findAllElemsOfType(classTag[SimpleNameTest]).map(_.name).toSet
    }

    assertResult(Set(QNameAsEQName("p:c"), QNameAsEQName("p:d"))) {
      topmostExprSingles(1).findAllElemsOfType(classTag[SimpleNameTest]).map(_.name).toSet
    }
  }

  test("testLetExpr") {
    // Example from the XPath 3.0 spec

    val exprString =
      """let $f := function($a) { starts-with($a, "E") }
        return local:filter(("Ethel", "Enid", "Gertrude"), $f)"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    val letExprOption = parseResult.get.value.findFirstElemOrSelfOfType(classTag[LetExpr])

    assertResult(true) {
      letExprOption.isDefined
    }

    assertResult(true) {
      letExprOption.get.simpleLetBindings.head.expr.findFirstElemOrSelfOfType(classTag[InlineFunctionExpr]).nonEmpty
    }

    assertResult(1) {
      letExprOption.get.returnExpr.findAllElemsOrSelfOfType(classTag[FunctionCall]).size
    }

    assertResult(Set(QNameAsEQName("a"), QNameAsEQName("f"))) {
      letExprOption.get.findAllElemsOrSelfOfType(classTag[VarRef]).map(_.varName).toSet
    }

    assertResult(Set("E", "Ethel", "Enid", "Gertrude")) {
      letExprOption.get.findAllElemsOrSelfOfType(classTag[StringLiteral]).map(_.value).toSet
    }
  }

  test("testSimplePathExpr") {
    // Example from https://en.wikibooks.org/wiki/XQuery/XPath_examples

    val exprString = "$books//book[contains(title, 'XQuery')]/title/text()"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(4) {
      parseResult.get.value.findAllElemsOfType(classTag[AxisStep]).size
    }

    assertResult(4) {
      parseResult.get.value.findAllElemsOfType(classTag[ForwardAxisStep]).size
    }

    assertResult(4) {
      parseResult.get.value.findAllElemsOfType(classTag[AbbrevForwardStep]).size
    }
  }

  test("testIncorrectSimplePathExpr") {
    // Example from https://en.wikibooks.org/wiki/XQuery/XPath_examples, but adapted

    val exprString = "$books///book[contains(title, 'XQuery')]/title/text()"

    val parseResult = xpathExpr.parse(exprString)

    assertFailure(parseResult)
  }

  test("testIncorrectSimplePathExprWithTwoCommas") {
    // Example from https://en.wikibooks.org/wiki/XQuery/XPath_examples, but adapted

    val exprString = "$books//book[contains(title,, 'XQuery')]/title/text()"

    val parseResult = xpathExpr.parse(exprString)

    assertFailure(parseResult)
  }

  test("testSimplePathExprWithEscapeInStringLiteral") {
    // Example from https://en.wikibooks.org/wiki/XQuery/XPath_examples, but adapted

    val exprString = "$books//book[contains(description, 'A ''fine book''')]/title/text()"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(4) {
      parseResult.get.value.findAllElemsOfType(classTag[AxisStep]).size
    }

    assertResult(List("A 'fine book'")) {
      parseResult.get.value.findAllElemsOfType(classTag[StringLiteral]).map(_.value)
    }
  }

  test("testSimplePathExprWithIncorrectlyEscapeInStringLiteral") {
    // Example from https://en.wikibooks.org/wiki/XQuery/XPath_examples, but adapted

    val exprString = "$books//book[contains(description, 'A 'fine book'')]/title/text()"

    val parseResult = xpathExpr.parse(exprString)

    assertFailure(parseResult)
  }

  test("testNonTrivialForExpr") {
    // Example from https://github.com/Saxonica/XT-Speedo
    // This one helped find and solve a few bugs in the parser.

    val exprString = """for $w in //text()/tokenize(., '\W+')[.!=''] return lower-case($w)"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[ForExpr]).size
    }

    val forExpr = parseResult.get.value.findAllElemsOrSelfOfType(classTag[ForExpr]).head

    assertResult(2) {
      forExpr.simpleForBindings.head.expr.findAllTopmostElemsOrSelfOfType(classTag[StepExpr]).size
    }

    val predicate =
      forExpr.simpleForBindings.head.expr.findAllElemsOrSelfOfType(classTag[Predicate]).head

    assertResult(true) {
      predicate.findFirstElemOfType(classTag[ContextItemExpr.type]).nonEmpty
    }

    assertResult(Some("")) {
      predicate.findFirstElemOfType(classTag[StringLiteral]).map(_.value)
    }
  }

  // TODO Many tests with syntactically incorrect XPath expressions

  private def assertSuccess(parseResult: Parsed[_]): Unit = {
    assertResult(true) {
      parseResult.fold(
        (parser, pos, extra) => false,
        (expr, pos) => true)
    }
  }

  private def assertFailure(parseResult: Parsed[_]): Unit = {
    assertResult(false) {
      parseResult.fold(
        (parser, pos, extra) => false,
        (expr, pos) => true)
    }
  }
}
