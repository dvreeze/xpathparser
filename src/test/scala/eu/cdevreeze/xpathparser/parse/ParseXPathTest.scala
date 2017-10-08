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

import eu.cdevreeze.xpathparser.ast.AbbrevForwardStep
import eu.cdevreeze.xpathparser.ast.AbbrevReverseStep
import eu.cdevreeze.xpathparser.ast.AdditionOp
import eu.cdevreeze.xpathparser.ast.AnyWildcard
import eu.cdevreeze.xpathparser.ast.ArgumentList
import eu.cdevreeze.xpathparser.ast.ArrowExpr
import eu.cdevreeze.xpathparser.ast.AxisStep
import eu.cdevreeze.xpathparser.ast.ContextItemExpr
import eu.cdevreeze.xpathparser.ast.CurlyArrayConstructor
import eu.cdevreeze.xpathparser.ast.EQName
import eu.cdevreeze.xpathparser.ast.ExprSingle
import eu.cdevreeze.xpathparser.ast.ForExpr
import eu.cdevreeze.xpathparser.ast.ForwardAxis
import eu.cdevreeze.xpathparser.ast.ForwardAxisStep
import eu.cdevreeze.xpathparser.ast.FunctionCall
import eu.cdevreeze.xpathparser.ast.GeneralComp
import eu.cdevreeze.xpathparser.ast.IfExpr
import eu.cdevreeze.xpathparser.ast.InlineFunctionExpr
import eu.cdevreeze.xpathparser.ast.IntegerLiteral
import eu.cdevreeze.xpathparser.ast.LetExpr
import eu.cdevreeze.xpathparser.ast.LocalNameWildcard
import eu.cdevreeze.xpathparser.ast.MapConstructorEntry
import eu.cdevreeze.xpathparser.ast.NamedFunctionRef
import eu.cdevreeze.xpathparser.ast.NamespaceWildcard
import eu.cdevreeze.xpathparser.ast.NonAbbrevForwardStep
import eu.cdevreeze.xpathparser.ast.NonAbbrevReverseStep
import eu.cdevreeze.xpathparser.ast.OneOrMoreSequenceType
import eu.cdevreeze.xpathparser.ast.ParenthesizedExpr
import eu.cdevreeze.xpathparser.ast.PostfixLookup
import eu.cdevreeze.xpathparser.ast.Predicate
import eu.cdevreeze.xpathparser.ast.PrefixWildcard
import eu.cdevreeze.xpathparser.ast.ReverseAxis
import eu.cdevreeze.xpathparser.ast.SimpleNameTest
import eu.cdevreeze.xpathparser.ast.SquareArrayConstructor
import eu.cdevreeze.xpathparser.ast.StepExpr
import eu.cdevreeze.xpathparser.ast.StringLiteral
import eu.cdevreeze.xpathparser.ast.UnaryOp
import eu.cdevreeze.xpathparser.ast.ValueComp
import eu.cdevreeze.xpathparser.ast.VarRef
import eu.cdevreeze.xpathparser.common.EName

/**
 * XPath parsing test case.
 *
 * Some sources for test XPath expressions were:
 * <ul>
 * <li>https://github.com/Saxonica/XT-Speedo</li>
 * <li>https://en.wikibooks.org/wiki/XQuery/XPath</li>
 * <li>https://www.w3.org/TR/xpath-30</li>
 * <li>https://www.w3.org/TR/xpath-31/</li>
 * <li>http://www.nltaxonomie.nl/nt12/kvk/</li>
 * <li>https://dev.w3.org/2011/QT3-test-suite</li>
 * <li>https://www.progress.com/tutorials/xquery/tour</li>
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
      EQName.QName("p:a"),
      EQName.QName("p:b"),
      EQName.QName("p:c"),
      EQName.QName("p:d"),
      EQName.QName("p:e"))) {

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

    assertResult(List(EQName.QName("varArc_BalanceSheetVertical_MsgPrecondValueConceptAndNoExistenceConcept1_ResultForTheYear"))) {
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
      EQName.QName("varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_SumOfMembers"),
      EQName.QName("varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_ChildrenMember"))) {

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

    assertResult(List(EQName.QName("varArc_DocumentInformation_MsgPrecondExistenceMemberAspect3_AllItems"))) {
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
      EQName.QName("xfi:is-instant-period"),
      EQName.QName("xfi:period"),
      EQName.QName("xfi:period-instant"),
      EQName.QName("xs:dateTime"),
      EQName.QName("xs:dayTimeDuration"),
      EQName.QName("xfi:is-start-end-period"),
      EQName.QName("xfi:period-end"),
      EQName.QName("xfi:period-start"),
      EQName.QName("false"))) {

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

    assertResult(Set(EQName.QName("varArc_EntityInformation_MsgEqualToIdentifierScheme1_AllItems"))) {
      parseResult.get.value.findAllElemsOfType(classTag[VarRef]).map(_.varName).toSet
    }

    assertResult(Set(EQName.QName("xfi:identifier"), EQName.QName("xfi:identifier-scheme"))) {
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

    assertResult(Set(EQName.QName("varArc_EntityInformation_MsgEqualToIdentifierScheme1_AllItems"))) {
      parseResult.get.value.findAllElemsOfType(classTag[VarRef]).map(_.varName).toSet
    }

    assertResult(Set(EQName.URIQualifiedName(EName(XfiNs, "identifier")), EQName.URIQualifiedName(EName(XfiNs, "identifier-scheme")))) {
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

    assertResult(Set(EQName.QName("p:a"), EQName.QName("p:b"), EQName.QName("xlink:type"))) {
      topmostExprSingles(0).findAllElemsOfType(classTag[SimpleNameTest]).map(_.name).toSet
    }

    assertResult(Set(EQName.QName("p:c"), EQName.QName("p:d"))) {
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

    assertResult(Set(EQName.QName("a"), EQName.QName("f"))) {
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

  test("testSimpleNonAbbrevSteps") {
    val exprString =
      "//a/child::b/following-sibling::d[@id = $id1]/child::c/../preceding::y/e[@id = $id2]/ancestor::x/child::title/text()"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(Set(EQName.QName("id1"), EQName.QName("id2"))) {
      parseResult.get.value.findAllElemsOfType(classTag[VarRef]).map(_.varName).toSet
    }

    assertResult(List(ForwardAxis.Child, ForwardAxis.FollowingSibling, ForwardAxis.Child, ForwardAxis.Child)) {
      parseResult.get.value.findAllElemsOfType(classTag[NonAbbrevForwardStep]).map(_.forwardAxis)
    }
    assertResult(List(ReverseAxis.Preceding, ReverseAxis.Ancestor)) {
      parseResult.get.value.findAllElemsOfType(classTag[NonAbbrevReverseStep]).map(_.reverseAxis)
    }

    assertResult(List(EQName.QName("a"), EQName.QName("id"), EQName.QName("e"), EQName.QName("id"), EQName.QName("unknown"))) {
      parseResult.get.value.findAllElemsOfType(classTag[AbbrevForwardStep]).map(_.nodeTest) map {
        case SimpleNameTest(eqName) => eqName
        case _                      => EQName.QName("unknown")
      }
    }
    assertResult(1) {
      parseResult.get.value.findAllElemsOfType(classTag[AbbrevReverseStep.type]).size
    }
  }

  test("testOccurrenceIndicator") {
    val exprString = "4 treat as item() + - 5"

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOfType(classTag[OneOrMoreSequenceType]).size
    }
  }

  // Expression "function () as xs:string *" does not work yet. See xgc:occurrence-indicators.

  test("testMultiplePredicates") {
    // Example from https://github.com/Saxonica/XT-Speedo

    val exprString =
      """xhtml:span[ancestor::xhtml:p | ancestor::xhtml:div][not(contains(@style, 'mso-list:'))]"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[AxisStep]).size
    }
    assertResult(2) {
      val axisStep = parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[AxisStep]).head
      axisStep.predicateList.size
    }
  }

  test("testNamedFunctionRef") {
    // Example from https://dev.w3.org/2011/QT3-test-suite/, test case function-literal-008

    val exprString =
      """Q{http://www.w3.org/2005/xpath-functions}nilled#1(/root)"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[NamedFunctionRef]).size
    }
  }

  test("testMapOfWeekdays") {
    // Example from the XPath 3.1 spec

    val exprString =
      """map {
  "Su" : "Sunday",
  "Mo" : "Monday",
  "Tu" : "Tuesday",
  "We" : "Wednesday",
  "Th" : "Thursday",
  "Fr" : "Friday",
  "Sa" : "Saturday"
}"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(7) {
      parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).size
    }
  }

  test("testWrongMapOfWeekdays") {
    // Example from the XPath 3.1 spec, adapted

    val exprString =
      """map {
  "Su" : "Sunday",
  "Mo" : "Monday",
  "Tu" : "Tuesday",
  "We" : "Wednesday",
  "Th" : "Thursday",
  "Fr" : "Friday",
  "Sa"
}"""

    val parseResult = xpathExpr.parse(exprString)

    assertFailure(parseResult)
  }

  test("testNestedMap") {
    // Example from the XPath 3.1 spec

    val exprString =
      """map {
    "book": map {
        "title": "Data on the Web",
        "year": 2000,
        "author": [
            map {
                "last": "Abiteboul",
                "first": "Serge"
            },
            map {
                "last": "Buneman",
                "first": "Peter"
            },
            map {
                "last": "Suciu",
                "first": "Dan"
            }
        ],
        "publisher": "Morgan Kaufmann Publishers",
        "price": 39.95
    }
}
"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).size
    }

    assertResult(Some("book")) {
      val firstKey =
        parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).head.keyExpr

      firstKey.findFirstElemOfType(classTag[StringLiteral]).map(_.value)
    }

    assertResult(12) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[MapConstructorEntry]).size
    }
  }

  test("testMapOfWeekdaysPseudoFunctionCall") {
    // Example from the XPath 3.1 spec, adapted to become a "function call"

    val exprString =
      """map {
  "Su" : "Sunday",
  "Mo" : "Monday",
  "Tu" : "Tuesday",
  "We" : "Wednesday",
  "Th" : "Thursday",
  "Fr" : "Friday",
  "Sa" : "Saturday"
}("Su")"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[ArgumentList]).size
    }
  }

  test("testMapPseudoFunctionCalls") {
    // Example from the XPath 3.1 spec

    val exprString = """$b("book")("author")(1)("last")"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(4) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[ArgumentList]).size
    }
  }

  test("testPrefixWildcard") {
    // There seems to be no test for prefix wildcards in test suite https://dev.w3.org/2011/QT3-test-suite/.

    val exprString = """a:*"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[PrefixWildcard]).size
    }
  }

  test("testLocalNameWildcard") {
    val exprString = """*:a"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[LocalNameWildcard]).size
    }
  }

  test("testAnyWildcard") {
    val exprString = """*"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[AnyWildcard.type]).size
    }
  }

  test("testNamespaceWildcard") {
    val exprString = """Q{http://www.example.org/customnamespace}*"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[NamespaceWildcard]).size
    }

    assertResult(Some("http://www.example.org/customnamespace")) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[NamespaceWildcard]).head.bracedUriLiteral.namespaceOption
    }
  }

  test("testEmptyNamespaceWildcard") {
    val exprString = """Q{}*"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[NamespaceWildcard]).size
    }

    assertResult(None) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[NamespaceWildcard]).head.bracedUriLiteral.namespaceOption
    }
  }

  test("testWrongMapConstructor") {
    // Example from the XPath 3.1 spec

    val exprString = """map{a:b}"""

    val parseResult = xpathExpr.parse(exprString)

    assertFailure(parseResult)
  }

  test("testMapConstructorUsingSpaceForDisambiguation") {
    // Example from the XPath 3.1 spec

    val exprString = """map{a : b}"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).size
    }

    assertResult(parseResult.get.value) {
      xpathExpr.parse("""map{a: b}""").get.value
    }

    assertResult(parseResult.get.value) {
      xpathExpr.parse("""map{a :b}""").get.value
    }
  }

  test("testMapConstructorWithQNameKey") {
    // Example from the XPath 3.1 spec

    val exprString = """map{a:b:c}"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).size
    }

    assertResult(Some(EQName.QName("a:b"))) {
      val firstKey =
        parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).head.keyExpr

      firstKey.findFirstElemOfType(classTag[SimpleNameTest]).map(_.name)
    }

    assertResult(parseResult.get.value) {
      xpathExpr.parse("""map{a:b: c}""").get.value
    }

    assertResult(parseResult.get.value) {
      xpathExpr.parse("""map{a:b :c}""").get.value
    }
  }

  test("testMapConstructorWithOtherQNameKey") {
    // Example from the XPath 3.1 spec, adapted

    val exprString = """map{a : b:c}"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).size
    }

    assertResult(Some(EQName.QName("a"))) {
      val firstKey =
        parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).head.keyExpr

      firstKey.findFirstElemOfType(classTag[SimpleNameTest]).map(_.name)
    }

    assertResult(parseResult.get.value) {
      xpathExpr.parse("""map{a: b:c}""").get.value
    }

    assertResult(parseResult.get.value) {
      xpathExpr.parse("""map{a :b:c}""").get.value
    }
  }

  test("testMapConstructorWithPrefixWildcardKey") {
    // Example from the XPath 3.1 spec

    val exprString = """map{a:*:c}"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).size
    }

    assertResult(Some("a")) {
      val firstKey =
        parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).head.keyExpr

      firstKey.findFirstElemOfType(classTag[PrefixWildcard]).map(_.prefix.name)
    }

    assertResult(parseResult.get.value) {
      xpathExpr.parse("""map{a:*: c}""").get.value
    }

    assertResult(parseResult.get.value) {
      xpathExpr.parse("""map{a:* :c}""").get.value
    }
  }

  test("testMapConstructorWithPrefixWildcardValue") {
    // Example from the XPath 3.1 spec, adapted

    val exprString = """map{a : *:c}"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).size
    }

    assertResult(Some(EQName.QName("a"))) {
      val firstKey =
        parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).head.keyExpr

      firstKey.findFirstElemOfType(classTag[SimpleNameTest]).map(_.name)
    }

    assertResult(parseResult.get.value) {
      xpathExpr.parse("""map{a: *:c}""").get.value
    }

    // Note the delimiting terminal ":*", which makes the parsing fail

    assertFailure(xpathExpr.parse("""map{a :*:c}"""))
  }

  test("testMapConstructorWithLocalNameWildcardKey") {
    // Example from the XPath 3.1 spec

    val exprString = """map{*:b:c}"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).size
    }

    assertResult(Some("b")) {
      val firstKey =
        parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).head.keyExpr

      firstKey.findFirstElemOfType(classTag[LocalNameWildcard]).map(_.localName.name)
    }

    assertResult(parseResult.get.value) {
      xpathExpr.parse("""map{*:b: c}""").get.value
    }

    assertResult(parseResult.get.value) {
      xpathExpr.parse("""map{*:b :c}""").get.value
    }
  }

  test("testMapConstructorWithAnyWildcardKey") {
    // Example from the XPath 3.1 spec, adapted

    val exprString = """map{* : b:c}"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).size
    }

    assertResult(Some(AnyWildcard)) {
      val firstKey =
        parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[MapConstructorEntry]).head.keyExpr

      firstKey.findFirstElemOfType(classTag[AnyWildcard.type])
    }

    // Note the delimiting terminal "*:", which makes the parsing fail

    assertFailure(xpathExpr.parse("""map{*: b:c}"""))

    assertResult(parseResult.get.value) {
      xpathExpr.parse("""map{* :b:c}""").get.value
    }
  }

  test("testSquareArrayConstructor") {
    // Example from the XPath 3.1 spec, adapted

    val exprString =
      """[ $x, local:items(), (), (27, 17, 0) ]"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[SquareArrayConstructor]).size
    }

    assertResult(4) {
      parseResult.get.value.findFirstElemOrSelfOfType(classTag[SquareArrayConstructor]).get.members.size
    }
  }

  test("testCurlyArrayConstructor") {
    // Example from the XPath 3.1 spec, adapted

    val exprString =
      """array{ $x, local:items(), (), (27, 17, 0) }"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[CurlyArrayConstructor]).size
    }
  }

  test("testSquareArrayPseudoFunctionCalls") {
    // Example from the XPath 3.1 spec

    val exprString = """[ [1, 2, 3], [4, 5, 6]](2)(2)"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(2) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[ArgumentList]).size
    }
  }

  test("testCurlyArrayPseudoFunctionCalls") {
    // Example from the XPath 3.1 spec

    val exprString = """array { (), (27, 17, 0) }(2)"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[ArgumentList]).size
    }
  }

  test("testMapLookup") {
    // Example from the XPath 3.1 spec

    val exprString = """map { "first" : "Jenna", "last" : "Scott" }?first"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[PostfixLookup]).size
    }
  }

  test("testNestedArrayLookup") {
    // Example from the XPath 3.1 spec

    val exprString = """([1,2,3], [4,5,6])?2"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[PostfixLookup]).size
    }
  }

  test("testArrowExpr") {
    // Example from the XPath 3.1 spec

    val exprString = """$string => upper-case() => normalize-unicode() => tokenize("\s+")"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllTopmostElemsOrSelfOfType(classTag[ArrowExpr]).size
    }

    assertResult(3) {
      val arrowExprs = parseResult.get.value.findAllElemsOrSelfOfType(classTag[ArrowExpr])

      arrowExprs.flatMap(_.arrowFunctionCalls).size
    }
  }

  test("testExprWithNamespaceWildcard") {
    // Example from https://dev.w3.org/2011/QT3-test-suite/, test case eqname-018

    val exprString = """(//Q{http://www.example.com/AuctionWatch}Start)[1]/namespace::Q{}*/string()"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[NamespaceWildcard]).size
    }

    assertResult(None) {
      parseResult.get.value.findFirstElemOrSelfOfType(classTag[NamespaceWildcard]).get.bracedUriLiteral.namespaceOption
    }
  }

  test("testExprWithOtherNamespaceWildcard") {
    // Example from https://dev.w3.org/2011/QT3-test-suite/, test case eqname-018, but adapted in the namespace URI

    val exprString =
      """(//Q{http://www.example.com/AuctionWatch}Start)[1]/namespace::Q{http://www.example.com/customnamespace}*/string()"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[NamespaceWildcard]).size
    }

    assertResult(Some("http://www.example.com/customnamespace")) {
      parseResult.get.value.findFirstElemOrSelfOfType(classTag[NamespaceWildcard]).get.bracedUriLiteral.namespaceOption
    }
  }

  test("testParenthesizedExpr") {
    // Example from https://www.progress.com/tutorials/xquery/tour

    val exprString = """(doc("books.xml")/bib/book/author)[1]"""

    val parseResult = xpathExpr.parse(exprString)

    assertSuccess(parseResult)

    assertResult(1) {
      parseResult.get.value.findAllElemsOrSelfOfType(classTag[ParenthesizedExpr]).size
    }

    assertResult(4) {
      val parenExprs =
        parseResult.get.value.findAllElemsOrSelfOfType(classTag[ParenthesizedExpr])

      parenExprs.flatMap(_.findAllTopmostElemsOrSelfOfType(classTag[StepExpr])).size
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
