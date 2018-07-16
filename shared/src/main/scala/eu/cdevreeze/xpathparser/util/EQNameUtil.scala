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

import eu.cdevreeze.xpathparser.ast.ArgumentList
import eu.cdevreeze.xpathparser.ast.AtomicOrUnionType
import eu.cdevreeze.xpathparser.ast.AttributeNameAndTypeTest
import eu.cdevreeze.xpathparser.ast.AttributeNameTest
import eu.cdevreeze.xpathparser.ast.AttributeTypeTest
import eu.cdevreeze.xpathparser.ast.EQName
import eu.cdevreeze.xpathparser.ast.EQNameAsArrowFunctionSpecifier
import eu.cdevreeze.xpathparser.ast.ElementNameAndTypeTest
import eu.cdevreeze.xpathparser.ast.ElementNameTest
import eu.cdevreeze.xpathparser.ast.ElementTypeTest
import eu.cdevreeze.xpathparser.ast.ExprSingleArgument
import eu.cdevreeze.xpathparser.ast.FunctionCall
import eu.cdevreeze.xpathparser.ast.NamedFunctionRef
import eu.cdevreeze.xpathparser.ast.NillableElementNameAndTypeTest
import eu.cdevreeze.xpathparser.ast.NillableElementTypeTest
import eu.cdevreeze.xpathparser.ast.NonEmptySingleType
import eu.cdevreeze.xpathparser.ast.Param
import eu.cdevreeze.xpathparser.ast.PotentiallyEmptySingleType
import eu.cdevreeze.xpathparser.ast.SchemaAttributeTest
import eu.cdevreeze.xpathparser.ast.SchemaElementTest
import eu.cdevreeze.xpathparser.ast.SimpleNameTest
import eu.cdevreeze.xpathparser.ast.StringLiteral
import eu.cdevreeze.xpathparser.ast.VarRef
import eu.cdevreeze.xpathparser.ast.VariableBinding
import eu.cdevreeze.xpathparser.ast.XPathElem
import eu.cdevreeze.xpathparser.ast.XPathExpr
import eu.cdevreeze.xpathparser.common.PrefixedName
import eu.cdevreeze.xpathparser.common.QName

/**
 * Utility to query for used EQNames (and namespace prefixes in those EQNames that are QNames).
 *
 * This utility can be handy when searching for all namespace prefixes used in an XML document, if the document
 * can have XPath expressions in attribute values or element text according to the schema.
 *
 * @author Chris de Vreeze
 */
object EQNameUtil {

  /**
   * Finds the namespace prefixes used in QNames in the given XPath expression, taking the extra EQName producer into account.
   *
   * Note that this function does not look for namespace prefixes in (prefix) wildcards.
   *
   * In other words, returns `findUsedEQNames(expr, extraEQNameProducer).flatMap(n => findPrefix(n))`.
   */
  def findUsedPrefixes(expr: XPathExpr, extraEQNameProducer: XPathElem => Option[Set[EQName]]): Set[String] = {
    val prefixesInEQNames: Set[String] = findUsedEQNames(expr, extraEQNameProducer).flatMap(n => findPrefix(n))
    prefixesInEQNames
  }

  /**
   * Finds the namespace prefixes used in QNames in the given XPath expression.
   *
   * Note that this function does not look for namespace prefixes in (prefix) wildcards.
   *
   * In other words, returns `findUsedPrefixes(expr, e => None)`.
   */
  def findUsedPrefixes(expr: XPathExpr): Set[String] = {
    findUsedPrefixes(expr, e => None)
  }

  /**
   * Finds the EQNames used in the given XPath expression, taking the extra EQName producer into account.
   */
  def findUsedEQNames(expr: XPathExpr, extraEQNameProducer: XPathElem => Option[Set[EQName]]): Set[EQName] = {
    val extraEQNames = expr.findAllElemsOrSelf.flatMap(e => extraEQNameProducer(e).getOrElse(Set.empty)).toSet

    findUsedEQNames(expr).union(extraEQNames)
  }

  /**
   * Finds the EQNames used in the given XPath expression.
   *
   * They are the EQNames in variable-references, variable bindings, function names, types, etc.
   */
  def findUsedEQNames(expr: XPathExpr): Set[EQName] = {
    // EQNames in variable-refs (note it does not matter if the var-ref is free or bound!)

    val eqnamesInVarRefs: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[VarRef]).map(_.varName).toSet

    // EQNames in function names

    val eqnamesInNamesOfCalledFunctions: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[FunctionCall]).map(_.functionName).toSet

    val eqnamesInNamedFunctionRefs: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[NamedFunctionRef]).map(_.functionName).toSet

    // EQNames in "simple bindings"

    val eqnamesInVariableBindings: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[VariableBinding]).map(_.varName).toSet

    // EQNames in (names of) params, which concerns inline function expressions

    val eqnamesInParams: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[Param]).map(_.paramName).toSet

    // EQNames in name tests

    val eqnamesInSimpleNameTests: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[SimpleNameTest]).map(_.name).toSet

    // EQNames in arrow function specifiers

    val eqnamesInArrowFunctionSpecifiers: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[EQNameAsArrowFunctionSpecifier]).map(_.eqName).toSet

    // EQNames in types

    val eqnamesInTypes: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[AtomicOrUnionType]).map(_.tpe).toSet

    val eqnamesInNonEmptySingleTypes: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[NonEmptySingleType]).map(_.name).toSet

    val eqnamesInPotentiallyEmptySingleTypes: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[PotentiallyEmptySingleType]).map(_.name).toSet

    // EQNames in kind tests

    val eqnamesInAttributeNameTests: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[AttributeNameTest]).map(_.name).toSet

    val eqnamesInAttributeTypeTests: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[AttributeTypeTest]).map(_.tpe).toSet

    val eqnamesInAttributeNameAndTypeTests: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[AttributeNameAndTypeTest]).flatMap(t => List(t.name, t.tpe)).toSet

    val eqnamesInElementNameTests: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[ElementNameTest]).map(_.name).toSet

    val eqnamesInElementTypeTests: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[ElementTypeTest]).map(_.tpe).toSet

    val eqnamesInElementNameAndTypeTests: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[ElementNameAndTypeTest]).flatMap(t => List(t.name, t.tpe)).toSet

    val eqnamesInNillableElementTypeTests: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[NillableElementTypeTest]).map(_.tpe).toSet

    val eqnamesInNillableElementNameAndTypeTests: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[NillableElementNameAndTypeTest]).flatMap(t => List(t.name, t.tpe)).toSet

    val eqnamesInSchemaElementTests: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[SchemaElementTest]).map(_.name).toSet

    val eqnamesInSchemaAttributeTests: Set[EQName] =
      expr.findAllElemsOrSelfOfType(classTag[SchemaAttributeTest]).map(_.name).toSet

    eqnamesInVarRefs
      .union(eqnamesInNamesOfCalledFunctions)
      .union(eqnamesInNamedFunctionRefs)
      .union(eqnamesInVariableBindings)
      .union(eqnamesInParams)
      .union(eqnamesInSimpleNameTests)
      .union(eqnamesInArrowFunctionSpecifiers)
      .union(eqnamesInTypes)
      .union(eqnamesInNonEmptySingleTypes)
      .union(eqnamesInPotentiallyEmptySingleTypes)
      .union(eqnamesInAttributeNameTests)
      .union(eqnamesInAttributeTypeTests)
      .union(eqnamesInAttributeNameAndTypeTests)
      .union(eqnamesInElementNameTests)
      .union(eqnamesInElementTypeTests)
      .union(eqnamesInElementNameAndTypeTests)
      .union(eqnamesInNillableElementTypeTests)
      .union(eqnamesInNillableElementNameAndTypeTests)
      .union(eqnamesInSchemaElementTests)
      .union(eqnamesInSchemaAttributeTests)
  }

  /**
   * Returns the optional prefix of the given EQName, if it is a QName,
   * and returns None otherwise.
   */
  def findPrefix(eqname: EQName): Option[String] = {
    eqname match {
      case EQName.URIQualifiedName(_) => None
      case EQName.QName(qn) => qn.prefixOption
    }
  }

  /**
   * Extracts an EQName from expressions like `xs:QName('p:nm')`.
   */
  val eqnameProducerFromXsQName: XPathElem => Option[Set[EQName]] = {
    case FunctionCall(
      EQName.QName(PrefixedName("xs", "QName")),
      ArgumentList(Vector(ExprSingleArgument(StringLiteral(s))))) =>

      val qn = QName.parse(s)
      Some(Set(EQName.QName(qn)))
    case _: XPathElem =>
      None
  }
}
