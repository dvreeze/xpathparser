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

package eu.cdevreeze.xpathparser.ast

import scala.reflect.classTag

import eu.cdevreeze.xpathparser.ast.AstCreation
import org.scalatest.funsuite.AnyFunSuite

/**
 * XPath creation test.
 *
 * @author Chris de Vreeze
 */
class CreateAstTest extends AnyFunSuite:

  import AstCreation.*

  test("testCreateSimplePathExpr") {
    val expr: PathExpr =
      singleSlashPathExpr(
        axisStep(abbrevForwardStep(nameTest(EQName.parse("p:a")))),
        doubleSlashStep(
          axisStep(abbrevForwardStep(nameTest(EQName.parse("p:b"))))),
        singleSlashStep(
          axisStep(abbrevForwardStep(nameTest(EQName.parse("p:c"))))),
        doubleSlashStep(
          axisStep(abbrevForwardStep(nameTest(EQName.parse("p:d"))))),
        singleSlashStep(
          axisStep(abbrevForwardStep(nameTest(EQName.parse("p:e")))))
      )

    assertResult(5) {
      expr.findAllElemsOrSelfOfType(classTag[StepExpr]).size
    }

    val expectedEQNames: Set[EQName] = Set("p:a", "p:b", "p:c", "p:d", "p:e").map(EQName.parse)

    assertResult(expectedEQNames) {
      expr.findAllElemsOrSelfOfType(classTag[StepExpr])
        .flatMap(_.findAllElemsOfType(classTag[SimpleNameTest]))
        .map(_.name)
        .toSet
    }
  }
