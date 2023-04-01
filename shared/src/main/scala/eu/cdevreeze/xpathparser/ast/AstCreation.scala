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
