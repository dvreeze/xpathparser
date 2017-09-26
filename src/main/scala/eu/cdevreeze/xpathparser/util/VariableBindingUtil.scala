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

import scala.collection.immutable

import eu.cdevreeze.xpathparser.ast.EQName
import eu.cdevreeze.xpathparser.ast.InlineFunctionExpr
import eu.cdevreeze.xpathparser.ast.VarRef
import eu.cdevreeze.xpathparser.ast.VariableBinding
import eu.cdevreeze.xpathparser.ast.VariableIntroducingExpr
import eu.cdevreeze.xpathparser.ast.XPathElem

/**
 * Utility to query for free and bound variable occurrences. It takes (inline) function parameters into account as well.
 *
 * Note that in an XPath expression EQNames can play different roles: variable names (including function parameters),
 * function names, type names, and any kind of name in node tests (kind tests and name tests). This utility only cares
 * about variables (including function parameters).
 *
 * @author Chris de Vreeze
 */
object VariableBindingUtil {

  /**
   * Returns all VarRef elements that are bound, given the passed inherited "introduced" variables and the variable bindings
   * of and in the given element itself. Note that function parameters (in inline functions) must also be treated as "introduced" variables.
   * All bound VarRefs that are descendant-or-self elements of the parameter element are returned.
   *
   * The "introduced" variables are those in variable bindings as well as function parameters.
   */
  def findAllBoundVariables(elem: XPathElem, inheritedIntroducedVariables: Set[EQName]): immutable.IndexedSeq[VarRef] = {
    // Many recursive calls are done below

    elem match {
      case e @ VarRef(eqname) =>
        if (inheritedIntroducedVariables.contains(eqname)) immutable.IndexedSeq(e) else immutable.IndexedSeq()
      case e: VariableBinding =>
        findAllBoundVariables(e.expr, inheritedIntroducedVariables)
      case e: VariableIntroducingExpr =>
        val introducedVariables = inheritedIntroducedVariables.union(e.variableBindings.map(_.varName).toSet)

        val boundVariablesInBindings =
          e.variableBindings.flatMap(b => findAllBoundVariables(b.expr, inheritedIntroducedVariables))

        val boundVariablesUsingOwnBindings =
          e.childrenAffectedByOwnVariableBindings.flatMap(che => findAllBoundVariables(che, introducedVariables))

        boundVariablesInBindings ++ boundVariablesUsingOwnBindings
      case e @ InlineFunctionExpr(paramListOption, resultTypeOption, body) =>
        val introducedVariables = inheritedIntroducedVariables.union(paramListOption.toSeq.flatMap(_.params).map(_.paramName).toSet)

        findAllBoundVariables(body, introducedVariables)
      case e =>
        e.children.flatMap(che => findAllBoundVariables(che, inheritedIntroducedVariables))
    }
  }

  /**
   * Returns all VarRef elements that are free despite the passed inherited "introduced" variables and the variable bindings
   * of and in the given element itself. Note that function parameters (in inline functions) must also be treated as "introduced" variables.
   * All free VarRefs that are descendant-or-self elements of the parameter element are returned.
   *
   * The "introduced" variables are those in variable bindings as well as function parameters.
   */
  def findAllFreeVariables(elem: XPathElem, inheritedIntroducedVariables: Set[EQName]): immutable.IndexedSeq[VarRef] = {
    // Many recursive calls are done below

    elem match {
      case e @ VarRef(eqname) =>
        if (inheritedIntroducedVariables.contains(eqname)) immutable.IndexedSeq() else immutable.IndexedSeq(e)
      case e: VariableBinding =>
        findAllFreeVariables(e.expr, inheritedIntroducedVariables)
      case e: VariableIntroducingExpr =>
        val introducedVariables = inheritedIntroducedVariables.union(e.variableBindings.map(_.varName).toSet)

        val freeVariablesInBindings =
          e.variableBindings.flatMap(b => findAllFreeVariables(b.expr, inheritedIntroducedVariables))

        val freeVariablesUsingOwnBindings =
          e.childrenAffectedByOwnVariableBindings.flatMap(che => findAllFreeVariables(che, introducedVariables))

        freeVariablesInBindings ++ freeVariablesUsingOwnBindings
      case e @ InlineFunctionExpr(paramListOption, resultTypeOption, body) =>
        val introducedVariables = inheritedIntroducedVariables.union(paramListOption.toSeq.flatMap(_.params).map(_.paramName).toSet)

        findAllFreeVariables(body, introducedVariables)
      case e =>
        e.children.flatMap(che => findAllFreeVariables(che, inheritedIntroducedVariables))
    }
  }

  /**
   * Returns `findAllBoundVariables(elem, Set())`.
   */
  def findAllBoundVariables(elem: XPathElem): immutable.IndexedSeq[VarRef] = {
    findAllBoundVariables(elem, Set())
  }

  /**
   * Returns `findAllFreeVariables(elem, Set())`.
   */
  def findAllFreeVariables(elem: XPathElem): immutable.IndexedSeq[VarRef] = {
    findAllFreeVariables(elem, Set())
  }
}
