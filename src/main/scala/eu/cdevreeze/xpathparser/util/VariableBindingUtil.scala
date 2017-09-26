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

import eu.cdevreeze.xpathparser.ast._
import eu.cdevreeze.xpathparser.common._

/**
 * Utility to query for free and bound variable occurrences. It takes function parameters into account as well.
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
   * of and in the given element itself. Note that function parameters (for named and inline functions) must also
   * be treated as "introduced" variables.
   *
   * The "introduced" variables are those in variable bindings as well as function parameters.
   */
  def findAllBoundVariables(elem: XPathElem, inheritedIntroducedVariables: Set[EQName]): immutable.IndexedSeq[VarRef] = {
    val introducedVariables: Set[EQName] = inheritedIntroducedVariables.union(getOwnIntroducedVariables(elem))

    ???
  }

  /**
   * Returns all VarRef elements that are free despite the passed inherited "introduced" variables and the variable bindings
   * of and in the given element itself. Note that function parameters (for named and inline functions) must also
   * be treated as "introduced" variables.
   *
   * The "introduced" variables are those in variable bindings as well as function parameters.
   */
  def findAllFreeVariables(elem: XPathElem, inheritedIntroducedVariables: Set[EQName]): immutable.IndexedSeq[VarRef] = {
    val introducedVariables: Set[EQName] = inheritedIntroducedVariables.union(getOwnIntroducedVariables(elem))

    ???
  }

  /**
   * Returns the variables introduced by this element itself (ignoring inherited ones and those introduced by descendant
   * elements). As far as this function is concerned, variable bindings do not directly introduce any variables, but
   * contribute to the variables introduced by their containing expressions.
   */
  private def getOwnIntroducedVariables(elem: XPathElem): Set[EQName] = {
    val ownIntroducedVariables: Set[EQName] =
      elem match {
        case e: VariableBindingExpr =>
          e.variableBindings.map(_.varName).toSet
        case InlineFunctionExpr(paramListOption, _, _) =>
          paramListOption.toSeq.flatMap(_.params).map(_.paramName).toSet
        case _ =>
          Set()
      }

    ownIntroducedVariables
  }
}
