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

/**
 * Friendly AST creation API.
 *
 * @author
 *   Chris de Vreeze
 */
object AstCreation:

  // TODO Implement

  // Creating path expressions

  val slashOnlyPathExpr: PathExpr = SlashOnlyPathExpr

  def singleSlashPathExpr(
      firstStep: StepExpr,
      remainder: SingleStepNestedPathExpr*
  ): PathExpr =
    PathExprStartingWithSingleSlash(RelativePathExpr(firstStep, remainder.toIndexedSeq))

  def doubleSlashPathExpr(
      firstStep: StepExpr,
      remainder: SingleStepNestedPathExpr*
  ): PathExpr =
    PathExprStartingWithDoubleSlash(RelativePathExpr(firstStep, remainder.toIndexedSeq))

  def relativePathExpr(
      firstStep: StepExpr,
      remainder: SingleStepNestedPathExpr*
  ): PathExpr =
    RelativePathExpr(firstStep, remainder.toIndexedSeq)

  def singleSlashStep(stepExpr: StepExpr): SingleStepNestedPathExpr =
    SingleStepNestedPathExpr(StepOp.SingleSlash, stepExpr)

  def doubleSlashStep(stepExpr: StepExpr): SingleStepNestedPathExpr =
    SingleStepNestedPathExpr(StepOp.DoubleSlash, stepExpr)

  // Creating axis steps, which are step expressions

  def axisStep(forwardStep: ForwardStep, predicates: Predicate*): ForwardAxisStep =
    ForwardAxisStep(forwardStep, predicates.toIndexedSeq)

  def axisStep(reverseStep: ReverseStep, predicates: Predicate*): ReverseAxisStep =
    ReverseAxisStep(reverseStep, predicates.toIndexedSeq)

  def abbrevForwardStep(nodeTest: NodeTest): ForwardStep = SimpleAbbrevForwardStep(nodeTest)

  def abbrevAttrAxisForwardStep(nodeTest: NodeTest): ForwardStep = AttributeAxisAbbrevForwardStep(nodeTest)

  def forwardStep(forwardAxis: ForwardAxis, nodeTest: NodeTest): ForwardStep =
    NonAbbrevForwardStep(forwardAxis, nodeTest)

  val abbrevReverseStep: ReverseStep = AbbrevReverseStep

  def reverseStep(reverseAxis: ReverseAxis, nodeTest: NodeTest): ReverseStep =
    NonAbbrevReverseStep(reverseAxis, nodeTest)

  def nameTest(name: EQName): NodeTest = SimpleNameTest(name)

  def attrNameTest(name: EQName): NodeTest = AttributeNameTest(name)

  // For the other node tests, we need to use their classes/objects directly, with no further support being provided here
