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

import org.scalatest.funsuite.AnyFunSuite
import eu.cdevreeze.xpathparser.ast.EQName
import eu.cdevreeze.xpathparser.ast.ForExpr
import eu.cdevreeze.xpathparser.ast.InlineFunctionExpr
import eu.cdevreeze.xpathparser.ast.LetExpr
import eu.cdevreeze.xpathparser.ast.XPathElem
import eu.cdevreeze.xpathparser.ast.XPathExpr

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
class VariableBindingTest extends AnyFunSuite {

  import cats.parse.{Parser => P}

  import eu.cdevreeze.xpathparser.parse.XPathParser.xpathExpr

  private def throwNoExpr(): Nothing = sys.error(s"Could not parse expression")

  test("testBindingsInSlash") {
    val exprString = "/"

    val parseResult = xpathExpr.parseAll(exprString)

    assertSuccess(parseResult)

    assertFreeVariableNames(parseResult.getOrElse(throwNoExpr()), Set())
    assertBoundVariableNames(parseResult.getOrElse(throwNoExpr()), Set())
  }

  test("testBindingsInSimplePathExpr") {
    val exprString = "/p:a//p:b/p:c//p:d/p:e"

    val parseResult = xpathExpr.parseAll(exprString)

    assertSuccess(parseResult)

    assertFreeVariableNames(parseResult.getOrElse(throwNoExpr()), Set())
    assertBoundVariableNames(parseResult.getOrElse(throwNoExpr()), Set())
  }

  test("testBindingsInIfExprWithFunctionCalls") {
    // From the NL taxonomy (NT12)

    val exprString =
      "if(xff:has-fallback-value(xs:QName('varArc_BalanceSheetVertical_MsgPrecondValueConceptAndNoExistenceConcept1_ResultForTheYear'))) " +
        "then true() else not(count($varArc_BalanceSheetVertical_MsgPrecondValueConceptAndNoExistenceConcept1_ResultForTheYear) ge 1)"

    val parseResult = xpathExpr.parseAll(exprString)

    assertFreeVariableNames(
      parseResult.getOrElse(throwNoExpr()),
      Set(EQName.QName("varArc_BalanceSheetVertical_MsgPrecondValueConceptAndNoExistenceConcept1_ResultForTheYear")))
    assertBoundVariableNames(parseResult.getOrElse(throwNoExpr()), Set())
  }

  test("testBindingsInSummation") {
    // From the NL taxonomy (NT12)

    val exprString =
      "$varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_SumOfMembers =  " +
        "sum($varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_ChildrenMember) "

    val parseResult = xpathExpr.parseAll(exprString)

    assertFreeVariableNames(
      parseResult.getOrElse(throwNoExpr()),
      Set(
        EQName.QName(
          "varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_SumOfMembers"),
        EQName.QName(
          "varArc_NotesShareCapitalStatementOfChanges_MsgSeparateSumOfMembersOnAbstract1_Abstract_ChildrenMember")
      )
    )
    assertBoundVariableNames(parseResult.getOrElse(throwNoExpr()), Set())
  }

  test("testBindingsInIfExprWithFunctionCallsAndStringLiterals") {
    // From the NL taxonomy (NT12)

    val exprString =
      "xfi:fact-has-explicit-dimension-value($varArc_DocumentInformation_MsgPrecondExistenceMemberAspect3_AllItems," +
        "xs:QName('venj-bw2-dim:FinancialStatementsTypeAxis'),xs:QName('venj-bw2-dm:SeparateMember'))"

    val parseResult = xpathExpr.parseAll(exprString)

    assertSuccess(parseResult)

    assertFreeVariableNames(
      parseResult.getOrElse(throwNoExpr()),
      Set(EQName.QName("varArc_DocumentInformation_MsgPrecondExistenceMemberAspect3_AllItems")))
    assertBoundVariableNames(parseResult.getOrElse(throwNoExpr()), Set())
  }

  test("testLetExpr") {
    // Example from the XPath 3.0 spec

    val exprString =
      """let $f := function($a) { starts-with($a, "E") }
        return local:filter(("Ethel", "Enid", "Gertrude"), $f)"""

    val parseResult = xpathExpr.parseAll(exprString)

    assertSuccess(parseResult)

    assertFreeVariableNames(parseResult.getOrElse(throwNoExpr()), Set())
    assertBoundVariableNames(parseResult.getOrElse(throwNoExpr()), Set(EQName.QName("a"), EQName.QName("f")))

    val funcBody =
      parseResult.getOrElse(throwNoExpr()).findFirstElemOfType(classTag[InlineFunctionExpr]).get.body

    assertFreeVariableNames(funcBody, Set(EQName.QName("a")))
    assertBoundVariableNames(funcBody, Set())

    assertFreeVariableNames(funcBody, Set(), Set(EQName.QName("a")))
    assertBoundVariableNames(funcBody, Set(EQName.QName("a")), Set(EQName.QName("a")))

    val returnExpr = parseResult.getOrElse(throwNoExpr()).findFirstElemOrSelfOfType(classTag[LetExpr]).get.returnExpr

    assertFreeVariableNames(returnExpr, Set(EQName.QName("f")))
    assertBoundVariableNames(returnExpr, Set())

    assertFreeVariableNames(returnExpr, Set(), Set(EQName.QName("f")))
    assertBoundVariableNames(returnExpr, Set(EQName.QName("f")), Set(EQName.QName("f")))
  }

  test("testNonTrivialForExpr") {
    // Example from https://github.com/Saxonica/XT-Speedo

    val exprString = """for $w in //text()/tokenize(., '\W+')[.!=''] return lower-case($w)"""

    val parseResult = xpathExpr.parseAll(exprString)

    assertSuccess(parseResult)

    assertFreeVariableNames(parseResult.getOrElse(throwNoExpr()), Set())
    assertBoundVariableNames(parseResult.getOrElse(throwNoExpr()), Set(EQName.QName("w")))

    val returnExpr = parseResult.getOrElse(throwNoExpr()).findFirstElemOrSelfOfType(classTag[ForExpr]).get.returnExpr

    assertFreeVariableNames(returnExpr, Set(EQName.QName("w")))
    assertBoundVariableNames(returnExpr, Set())

    assertFreeVariableNames(returnExpr, Set(), Set(EQName.QName("w")))
    assertBoundVariableNames(returnExpr, Set(EQName.QName("w")), Set(EQName.QName("w")))
  }

  test("testLetExprWithFreeVars") {
    // Example from the XPath 3.0 spec, adapted to contain some free variables

    val exprString =
      """let $f := function($a) { starts-with($a, $b) }
        return local:filter(("Ethel", "Enid", "Gertrude", $c, $a), $f)"""

    val parseResult = xpathExpr.parseAll(exprString)

    assertSuccess(parseResult)

    assertFreeVariableNames(
      parseResult.getOrElse(throwNoExpr()),
      Set(EQName.QName("b"), EQName.QName("c"), EQName.QName("a")))
    assertBoundVariableNames(parseResult.getOrElse(throwNoExpr()), Set(EQName.QName("a"), EQName.QName("f")))

    val funcBody =
      parseResult.getOrElse(throwNoExpr()).findFirstElemOfType(classTag[InlineFunctionExpr]).get.body

    assertFreeVariableNames(funcBody, Set(EQName.QName("a"), EQName.QName("b")))
    assertBoundVariableNames(funcBody, Set())

    assertFreeVariableNames(funcBody, Set(EQName.QName("b")), Set(EQName.QName("a")))
    assertBoundVariableNames(funcBody, Set(EQName.QName("a")), Set(EQName.QName("a")))

    val returnExpr = parseResult.getOrElse(throwNoExpr()).findFirstElemOrSelfOfType(classTag[LetExpr]).get.returnExpr

    assertFreeVariableNames(returnExpr, Set(EQName.QName("f"), EQName.QName("c"), EQName.QName("a")))
    assertBoundVariableNames(returnExpr, Set())

    assertFreeVariableNames(returnExpr, Set(EQName.QName("c"), EQName.QName("a")), Set(EQName.QName("f")))
    assertBoundVariableNames(returnExpr, Set(EQName.QName("f")), Set(EQName.QName("f")))
  }

  test("testBindingsDependingOnOtherBindings") {
    // From the NL taxonomy (NT12)

    val exprString =
      """not((for
                $n in (1 to (count($varArc_ELRName_PrtFactsNECovA_Set01)-1)),
                $m in (($n+1) to (count($varArc_ELRName_PrtFactsNECovA_Set01)))
              return (($varArc_ELRName_PrtFactsNECovA_Set01[$n] = $varArc_ELRName_PrtFactsNECovA_Set01[$m]))) = true())""".trim

    val parseResult = xpathExpr.parseAll(exprString)

    // Note that $n is never free, not even in the variable binding for $m.

    assertFreeVariableNames(
      parseResult.getOrElse(throwNoExpr()),
      Set(EQName.QName("varArc_ELRName_PrtFactsNECovA_Set01")))
    assertBoundVariableNames(parseResult.getOrElse(throwNoExpr()), Set(EQName.QName("n"), EQName.QName("m")))

    val forExpr = parseResult.getOrElse(throwNoExpr()).findElemOfType(classTag[ForExpr])(_ => true).get
    val secondBindingExpr = forExpr.variableBindings.tail.head.expr

    assertFreeVariableNames(
      secondBindingExpr,
      Set(EQName.QName("n"), EQName.QName("varArc_ELRName_PrtFactsNECovA_Set01")))
    assertBoundVariableNames(secondBindingExpr, Set())

    assertFreeVariableNames(
      secondBindingExpr,
      Set(EQName.QName("varArc_ELRName_PrtFactsNECovA_Set01")),
      Set(EQName.QName("n"), EQName.QName("m")))
    assertBoundVariableNames(secondBindingExpr, Set(EQName.QName("n")), Set(EQName.QName("n"), EQName.QName("m")))

    val firstVarScope = forExpr.scopeOfVariableBinding(0)

    assertResult(2) {
      firstVarScope.size
    }
    assertResult(Set(secondBindingExpr, forExpr.returnExpr)) {
      firstVarScope.toSet
    }
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

  private def assertSuccess(parseResult: Either[P.Error, XPathExpr]): Unit = {
    assertResult(true, parseResult) {
      parseResult.isRight
    }
  }
}
