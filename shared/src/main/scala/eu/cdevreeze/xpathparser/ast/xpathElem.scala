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

import cats.data.NonEmptyVector
import eu.cdevreeze.xpathparser.queryapi.ElemLike

/**
 * XPath 3.1 AST. The root of the type hierarchy is XPathElem. It offers the ElemApi query API.
 *
 * The requirements on this AST are as follows:
 * <ul>
 * <li>It must represent the syntax tree of a successfully parsed XPath expression</li>
 * <li>It is not annotated with more semantic information, like type information that is not included in the XPath expression</li>
 * <li>It does not know anything about the context in which it runs, like bound namespaces etc.</li>
 * <li>It is rich enough to be able to serialize the AST back to XPath, knowing exactly where to place parentheses, braces, etc.</li>
 * <li>It should help evaluation, so it should model operator associativity and precedence correctly, where feasible</li>
 * <li>It must be readable in that object composition trees are not unnecessarily deep and therefore hard to comprehend</li>
 * <li>Serialization of the AST to XPath may lead to differences in whitespace (and operator aliases), but other than that the result must be the same</li>
 * <li>The AST class hierarchy does not have to use the exact same names as the XPath grammar</li>
 * <li>Instances of the AST classes are easy to create in an ad-hoc manner, so they contain no parsing state</li>
 * <li>The AST classes are case classes, with value equality and good support for pattern matching</li>
 * </ul>
 *
 * It would be natural for the AST types to have IS-A relationships modeled as type inheritance, and HAS-A
 * relationships modeled as composition. Where feasible, this approach has been followed. Consequently, sealed trait inheritance
 * has been used a lot (IS-A instead of HAS-A), and unnecessarily deep object trees are prevented in this way.
 *
 * Having such an AST of a successfully parsed XPath expression, it must be easy to reliably find used namespace prefixes, for example.
 *
 * The AST, as well as the parser, were developed using the XPath 3.1 specification as input. In particular, see
 * https://www.w3.org/TR/xpath-31/#id-grammar and https://www.w3.org/TR/xpath-31/#id-precedence-order. Note that the grammar
 * in isolation does not clearly convey left or right associativity of operators, so the extra information about operator
 * precedence and associativity is essential for understanding the "structure" of compound expressions.
 *
 * TODO Improve several class names.
 *
 * @author Chris de Vreeze
 */
sealed trait XPathElem extends ElemLike[XPathElem]:

  /**
   * Returns the (immediate) child elements of this element.
   */
  def children: IndexedSeq[XPathElem]

/**
 * Any leaf element in the AST of an XPath expression.
 */
sealed trait LeafElem extends XPathElem:

  final def children: IndexedSeq[XPathElem] = IndexedSeq()

/**
 * XPathElem that can introduce one or more variable bindings.
 */
sealed trait VariableIntroducingExpr extends XPathElem:

  /**
   * Returns the variable bindings introduced by this element.
   */
  def variableBindings: IndexedSeq[VariableBinding]

  /**
   * Returns the "return expression".
   */
  def returnExpr: XPathElem

  /**
   * Returns the scope of the variable binding whose index is given as parameter.
   * The scope determines where variables can be bound by that variable binding.
   */
  final def scopeOfVariableBinding(variableBindingIndex: Int): IndexedSeq[XPathElem] =
    require(
      0 <= variableBindingIndex && variableBindingIndex < variableBindings.size,
      s"Wrong variable binding index: $variableBindingIndex")

    variableBindings.drop(variableBindingIndex + 1).map(_.expr).appended(returnExpr)

/**
 * XPath expression.
 */
sealed trait XPathExpr extends XPathElem

// Enclosed expressions

/**
 * Optional expression enclosed in braces.
 */
final case class EnclosedExpr(exprOption: Option[Expr]) extends XPathElem:

  def children: IndexedSeq[XPathElem] = exprOption.toIndexedSeq

// Expressions

/**
 * Expression, which is a sequence of 1 or more ExprSingle objects, separated by commas.
 * The "comma" operator is associative, so the AST could have been left-associative or right-associative.
 */
sealed trait Expr extends XPathExpr

sealed trait SimpleExpr extends Expr

final case class CompoundExpr(firstExprSingle: ExprSingle, remainingExprSingleSeq: NonEmptyVector[ExprSingle])
    extends Expr:

  def exprSingleSeq: IndexedSeq[ExprSingle] = remainingExprSingleSeq.toVector.prepended(firstExprSingle)

  def children: IndexedSeq[XPathElem] = exprSingleSeq

object Expr:

  def apply(exprSingleSeq: NonEmptyVector[ExprSingle]): Expr =
    if exprSingleSeq.toVector.sizeIs == 1 then
      exprSingleSeq.head
    else
      assert(exprSingleSeq.toVector.sizeIs >= 2)
      CompoundExpr(exprSingleSeq.head, NonEmptyVector.fromVectorUnsafe(exprSingleSeq.tail))

/**
 * Expression-single, that is, an expression without any top-level commas. Most XPath expressions are expression-singles.
 */
sealed trait ExprSingle extends SimpleExpr

final case class ForExpr(simpleForBindings: NonEmptyVector[SimpleForBinding], returnExpr: ExprSingle)
    extends ExprSingle
    with VariableIntroducingExpr:

  def children: IndexedSeq[XPathElem] = simpleForBindings.toVector.appended(returnExpr)

  def variableBindings: IndexedSeq[VariableBinding] = simpleForBindings.toVector

final case class LetExpr(simpleLetBindings: NonEmptyVector[SimpleLetBinding], returnExpr: ExprSingle)
    extends ExprSingle
    with VariableIntroducingExpr:

  def children: IndexedSeq[XPathElem] = simpleLetBindings.toVector.appended(returnExpr)

  def variableBindings: IndexedSeq[VariableBinding] = simpleLetBindings.toVector

final case class QuantifiedExpr(
    quantifier: Quantifier,
    simpleBindings: NonEmptyVector[SimpleBindingInQuantifiedExpr],
    satisfiesExpr: ExprSingle)
    extends ExprSingle
    with VariableIntroducingExpr:

  def children: IndexedSeq[XPathElem] = simpleBindings.toVector.prepended(quantifier).appended(satisfiesExpr)

  def variableBindings: IndexedSeq[VariableBinding] = simpleBindings.toVector

  def returnExpr: XPathElem = satisfiesExpr

final case class IfExpr(condition: Expr, thenExpr: ExprSingle, elseExpr: ExprSingle) extends ExprSingle:

  def children: IndexedSeq[XPathElem] = IndexedSeq(condition, thenExpr, elseExpr)

/**
 * Or-expression. The "or" operator is associative, so the AST could have been left-associative or right-associative.
 */
sealed trait OrExpr extends ExprSingle

sealed trait SimpleOrExpr extends OrExpr

final case class CompoundOrExpr(firstAndExpr: AndExpr, remainingAndExprs: NonEmptyVector[AndExpr]) extends OrExpr:

  def andExprs: IndexedSeq[AndExpr] = remainingAndExprs.toVector.prepended(firstAndExpr)

  def children: IndexedSeq[XPathElem] = andExprs

object OrExpr:

  def apply(andExprs: NonEmptyVector[AndExpr]): OrExpr =
    if andExprs.toVector.sizeIs == 1 then
      andExprs.head
    else
      assert(andExprs.toVector.sizeIs >= 2)
      CompoundOrExpr(andExprs.head, NonEmptyVector.fromVectorUnsafe(andExprs.tail))

/**
 * And-expression. The "and" operator is associative, so the AST could have been left-associative or right-associative.
 */
sealed trait AndExpr extends SimpleOrExpr

sealed trait SimpleAndExpr extends AndExpr

final case class CompoundAndExpr(
    firstComparisonExpr: ComparisonExpr,
    remainingComparisonExprs: NonEmptyVector[ComparisonExpr])
    extends AndExpr:

  def comparisonExprs: IndexedSeq[ComparisonExpr] = remainingComparisonExprs.toVector.prepended(firstComparisonExpr)

  def children: IndexedSeq[XPathElem] = comparisonExprs

object AndExpr:

  def apply(comparisonExprs: NonEmptyVector[ComparisonExpr]): AndExpr =
    if comparisonExprs.toVector.sizeIs == 1 then
      comparisonExprs.head
    else
      assert(comparisonExprs.toVector.sizeIs >= 2)
      CompoundAndExpr(comparisonExprs.head, NonEmptyVector.fromVectorUnsafe(comparisonExprs.tail))

/**
 * Comparison expression, where the optional comparison operator is a value comparison operator,
 * general comparison operator or node comparison operator.
 */
sealed trait ComparisonExpr extends SimpleAndExpr

sealed trait SimpleComparisonExpr extends ComparisonExpr

final case class CompoundComparisonExpr(
    stringConcatExpr1: StringConcatExpr,
    comp: Comp,
    stringConcatExpr2: StringConcatExpr)
    extends ComparisonExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(stringConcatExpr1, comp, stringConcatExpr2)

/**
 * String concatenation expression, where the optional string concatenation uses the "||" operator.
 * String concatenation is left-associative.
 */
sealed trait StringConcatExpr extends SimpleComparisonExpr

sealed trait SimpleStringConcatExpr extends StringConcatExpr

final case class CompoundStringConcatExpr(firstRangeExpr: RangeExpr, remainingRangeExprs: NonEmptyVector[RangeExpr])
    extends StringConcatExpr:

  def rangeExprs: IndexedSeq[RangeExpr] = remainingRangeExprs.toVector.prepended(firstRangeExpr)

  def children: IndexedSeq[XPathElem] = rangeExprs

object StringConcatExpr:

  def apply(rangeExprs: NonEmptyVector[RangeExpr]): StringConcatExpr =
    if rangeExprs.toVector.sizeIs == 1 then
      rangeExprs.head
    else
      assert(rangeExprs.toVector.sizeIs >= 2)
      CompoundStringConcatExpr(rangeExprs.head, NonEmptyVector.fromVectorUnsafe(rangeExprs.tail))

/**
 * Range expression, where the optional range uses keyword "to" as operator.
 */
sealed trait RangeExpr extends SimpleStringConcatExpr

sealed trait SimpleRangeExpr extends RangeExpr

final case class CompoundRangeExpr(additiveExpr1: AdditiveExpr, additiveExpr2: AdditiveExpr) extends RangeExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(additiveExpr1, additiveExpr2)

/**
 * Additive expression. Addition/subtraction is left-associative.
 */
sealed trait AdditiveExpr extends SimpleRangeExpr

sealed trait SimpleAdditiveExpr extends AdditiveExpr

final case class CompoundAdditiveExpr(init: AdditiveExpr, op: AdditionOp, lastMultiplicativeExpr: MultiplicativeExpr)
    extends AdditiveExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(init, op, lastMultiplicativeExpr)

object AdditiveExpr:

  def apply(
      firstExpr: MultiplicativeExpr,
      operatorExprPairs: IndexedSeq[(AdditionOp, MultiplicativeExpr)]): AdditiveExpr =

    if operatorExprPairs.isEmpty then
      firstExpr
    else
      val (lastOp, lastExpr) = operatorExprPairs.last
      // Recursive call
      CompoundAdditiveExpr(apply(firstExpr, operatorExprPairs.init), lastOp, lastExpr)

/**
 * Multiplicative expression. The corresponding multiplication/division operators are left-associative.
 */
sealed trait MultiplicativeExpr extends SimpleAdditiveExpr

sealed trait SimpleMultiplicativeExpr extends MultiplicativeExpr

final case class CompoundMultiplicativeExpr(init: MultiplicativeExpr, op: MultiplicativeOp, lastUnionExpr: UnionExpr)
    extends MultiplicativeExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(init, op, lastUnionExpr)

object MultiplicativeExpr:

  def apply(firstExpr: UnionExpr, operatorExprPairs: IndexedSeq[(MultiplicativeOp, UnionExpr)]): MultiplicativeExpr =
    if operatorExprPairs.isEmpty then
      firstExpr
    else
      val (lastOp, lastExpr) = operatorExprPairs.last
      // Recursive call
      CompoundMultiplicativeExpr(apply(firstExpr, operatorExprPairs.init), lastOp, lastExpr)

/**
 * Union expression, where the optional union uses operator "union" or "|".
 * The union operator is associative, so the AST could have been left-associative or right-associative.
 */
sealed trait UnionExpr extends SimpleMultiplicativeExpr

sealed trait SimpleUnionExpr extends UnionExpr

final case class CompoundUnionExpr(
    firstIntersectExceptExpr: IntersectExceptExpr,
    remainingIntersectExceptExprs: NonEmptyVector[IntersectExceptExpr])
    extends UnionExpr:

  def intersectExceptExprs: IndexedSeq[IntersectExceptExpr] =
    remainingIntersectExceptExprs.toVector.prepended(firstIntersectExceptExpr)

  def children: IndexedSeq[XPathElem] = intersectExceptExprs

object UnionExpr:

  def apply(intersectExceptExprs: NonEmptyVector[IntersectExceptExpr]): UnionExpr =
    if intersectExceptExprs.toVector.sizeIs == 1 then
      intersectExceptExprs.head
    else
      assert(intersectExceptExprs.toVector.sizeIs >= 2)
      CompoundUnionExpr(intersectExceptExprs.head, NonEmptyVector.fromVectorUnsafe(intersectExceptExprs.tail))

/**
 * Intersect or except expression, optionally using the "intersect" or "except" operator. These operators are left-associative.
 */
sealed trait IntersectExceptExpr extends SimpleUnionExpr

sealed trait SimpleIntersectExceptExpr extends IntersectExceptExpr

final case class CompoundIntersectExceptExpr(
    init: IntersectExceptExpr,
    op: IntersectExceptOp,
    lastInstanceOfExpr: InstanceOfExpr)
    extends IntersectExceptExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(init, op, lastInstanceOfExpr)

object IntersectExceptExpr:

  def apply(
      firstExpr: InstanceOfExpr,
      operatorExprPairs: IndexedSeq[(IntersectExceptOp, InstanceOfExpr)]): IntersectExceptExpr =

    if operatorExprPairs.isEmpty then
      firstExpr
    else
      val (lastOp, lastExpr) = operatorExprPairs.last
      // Recursive call
      CompoundIntersectExceptExpr(apply(firstExpr, operatorExprPairs.init), lastOp, lastExpr)

sealed trait InstanceOfExpr extends SimpleIntersectExceptExpr

sealed trait SimpleInstanceOfExpr extends InstanceOfExpr

final case class CompoundInstanceOfExpr(treatExpr: TreatExpr, sequenceType: SequenceType) extends InstanceOfExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(treatExpr, sequenceType)

object InstanceOfExpr:

  def apply(treatExpr: TreatExpr, sequenceTypeOption: Option[SequenceType]): InstanceOfExpr =
    sequenceTypeOption.map(seqType => CompoundInstanceOfExpr(treatExpr, seqType)).getOrElse(treatExpr)

sealed trait TreatExpr extends SimpleInstanceOfExpr

sealed trait SimpleTreatExpr extends TreatExpr

final case class CompoundTreatExpr(castableExpr: CastableExpr, sequenceType: SequenceType) extends TreatExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(castableExpr, sequenceType)

object TreatExpr:

  def apply(castableExpr: CastableExpr, sequenceTypeOption: Option[SequenceType]): TreatExpr =
    sequenceTypeOption.map(seqType => CompoundTreatExpr(castableExpr, seqType)).getOrElse(castableExpr)

sealed trait CastableExpr extends SimpleTreatExpr

sealed trait SimpleCastableExpr extends CastableExpr

final case class CompoundCastableExpr(castExpr: CastExpr, singleType: SingleType) extends CastableExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(castExpr, singleType)

object CastableExpr:

  def apply(castExpr: CastExpr, singleTypeOption: Option[SingleType]): CastableExpr =
    singleTypeOption.map(singleType => CompoundCastableExpr(castExpr, singleType)).getOrElse(castExpr)

sealed trait CastExpr extends SimpleCastableExpr

sealed trait SimpleCastExpr extends CastExpr

final case class CompoundCastExpr(arrowExpr: ArrowExpr, singleType: SingleType) extends CastExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(arrowExpr, singleType)

object CastExpr:

  def apply(arrowExpr: ArrowExpr, singleTypeOption: Option[SingleType]): CastExpr =
    singleTypeOption.map(singleType => CompoundCastExpr(arrowExpr, singleType)).getOrElse(arrowExpr)

/**
 * Arrow expression. The arrow operator is left-associative.
 */
sealed trait ArrowExpr extends SimpleCastExpr

sealed trait SimpleArrowExpr extends ArrowExpr

final case class CompoundArrowExpr(unaryExpr: UnaryExpr, arrowFunctionCalls: NonEmptyVector[ArrowFunctionCall])
    extends ArrowExpr:

  def children: IndexedSeq[XPathElem] = arrowFunctionCalls.toVector.prepended(unaryExpr)

object ArrowExpr:

  def apply(unaryExpr: UnaryExpr, arrowFunctionCalls: IndexedSeq[ArrowFunctionCall]): ArrowExpr =
    if arrowFunctionCalls.isEmpty then
      unaryExpr
    else
      CompoundArrowExpr(unaryExpr, NonEmptyVector.fromVectorUnsafe(arrowFunctionCalls.toVector))

final case class ArrowFunctionCall(arrowFunctionSpecifier: ArrowFunctionSpecifier, argumentList: ArgumentList)
    extends XPathElem:

  def children: IndexedSeq[XPathElem] = IndexedSeq(arrowFunctionSpecifier, argumentList)

// Note that an arrow function specifier should extend Expr indirectly, but then an EQName must be an expression too

sealed trait ArrowFunctionSpecifier extends XPathElem

final case class EQNameAsArrowFunctionSpecifier(eqName: EQName) extends ArrowFunctionSpecifier with LeafElem

final case class VarRefAsArrowFunctionSpecifier(varRef: VarRef) extends ArrowFunctionSpecifier:

  def children: IndexedSeq[XPathElem] = IndexedSeq(varRef)

final case class ParenthesizedExprAsArrowFunctionSpecifier(parenthesizedExpr: ParenthesizedExpr)
    extends ArrowFunctionSpecifier:

  def children: IndexedSeq[XPathElem] = IndexedSeq(parenthesizedExpr)

/**
 * Unary expression. The corresponding unary operators are right-associative.
 */
sealed trait UnaryExpr extends SimpleArrowExpr

sealed trait SimpleUnaryExpr extends UnaryExpr

final case class CompoundUnaryExpr(ops: NonEmptyVector[UnaryOp], valueExpr: ValueExpr) extends UnaryExpr:

  def children: IndexedSeq[XPathElem] = ops.toVector.appended(valueExpr)

object UnaryExpr:

  def apply(ops: IndexedSeq[UnaryOp], valueExpr: ValueExpr): UnaryExpr =
    if ops.isEmpty then
      valueExpr
    else
      CompoundUnaryExpr(NonEmptyVector.fromVectorUnsafe(ops.toVector), valueExpr)

sealed trait ValueExpr extends SimpleUnaryExpr

/**
 * Simple map expression, using the optional map operator ("!"). This operator is left-associative.
 */
sealed trait SimpleMapExpr extends ValueExpr

sealed trait SimpleSimpleMapExpr extends SimpleMapExpr

final case class CompoundSimpleMapExpr(firstPathExpr: PathExpr, remainingPathExprs: NonEmptyVector[PathExpr])
    extends SimpleMapExpr:

  def pathExprs: IndexedSeq[PathExpr] = remainingPathExprs.toVector.prepended(firstPathExpr)

  def children: IndexedSeq[XPathElem] = pathExprs

object SimpleMapExpr:

  def apply(pathExprs: NonEmptyVector[PathExpr]): SimpleMapExpr =
    if pathExprs.toVector.sizeIs == 1 then
      pathExprs.head
    else
      assert(pathExprs.toVector.sizeIs >= 2)
      CompoundSimpleMapExpr(pathExprs.head, NonEmptyVector.fromVectorUnsafe(pathExprs.tail))

// Path and step expressions

/**
 * Path expression, so a relative path expression possibly preceded by "/" or "//" (or the expression "/" itself).
 * Path expressions are used to locate nodes within trees.
 */
sealed trait PathExpr extends SimpleSimpleMapExpr

case object SlashOnlyPathExpr extends PathExpr with LeafElem

final case class PathExprStartingWithSingleSlash(relativePathExpr: RelativePathExpr) extends PathExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(relativePathExpr)

final case class PathExprStartingWithDoubleSlash(relativePathExpr: RelativePathExpr) extends PathExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(relativePathExpr)

/**
 * Relative path expression, consisting of a number of step expressions separated by step operators ("/" and "//").
 * These step operators are left-associative.
 */
sealed trait RelativePathExpr extends PathExpr

sealed trait SimpleRelativePathExpr extends RelativePathExpr

final case class CompoundRelativePathExpr(init: RelativePathExpr, op: StepOp, lastStepExpr: StepExpr)
    extends RelativePathExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(init, op, lastStepExpr)

object RelativePathExpr:

  def apply(firstExpr: StepExpr, operatorExprPairs: IndexedSeq[(StepOp, StepExpr)]): RelativePathExpr =
    if operatorExprPairs.isEmpty then
      firstExpr
    else
      val (lastOp, lastExpr) = operatorExprPairs.last
      // Recursive call
      CompoundRelativePathExpr(apply(firstExpr, operatorExprPairs.init), lastOp, lastExpr)

/**
 * Single step in an absolute or relative path expression. Note that step expressions are either
 * postfix expressions or axis steps.
 */
sealed trait StepExpr extends SimpleRelativePathExpr

/**
 * Postfix expression, which is a primary expression succeeded by 0 or more predicates, arguments lists
 * and/or lookups. Note that lookup (the "?" operator) is left-associative.
 */
sealed trait PostfixExpr extends StepExpr

sealed trait SimplePostfixExpr extends PostfixExpr

final case class CompoundPostfixExpr(primaryExpr: PrimaryExpr, postfixes: NonEmptyVector[Postfix]) extends PostfixExpr:

  def children: IndexedSeq[XPathElem] = postfixes.toVector.prepended(primaryExpr)

object PostfixExpr:

  def apply(primaryExpr: PrimaryExpr, postfixes: IndexedSeq[Postfix]): PostfixExpr =
    if postfixes.isEmpty then
      primaryExpr
    else
      CompoundPostfixExpr(primaryExpr, NonEmptyVector.fromVectorUnsafe(postfixes.toVector))

/**
 * Axis step. For example: "child::book[@pageCount > 800]". The "[]" operator for adding predicates is left-associative.
 */
sealed trait AxisStep extends StepExpr:

  def predicateList: IndexedSeq[Predicate]

final case class ForwardAxisStep(step: ForwardStep, predicateList: IndexedSeq[Predicate]) extends AxisStep:

  def children: IndexedSeq[XPathElem] = predicateList.prepended(step)

final case class ReverseAxisStep(step: ReverseStep, predicateList: IndexedSeq[Predicate]) extends AxisStep:

  def children: IndexedSeq[XPathElem] = predicateList.prepended(step)

sealed trait ForwardStep extends XPathElem:

  def nodeTest: NodeTest

final case class NonAbbrevForwardStep(forwardAxis: ForwardAxis, nodeTest: NodeTest) extends ForwardStep:

  def children: IndexedSeq[XPathElem] = IndexedSeq(forwardAxis, nodeTest)

sealed trait AbbrevForwardStep extends ForwardStep

final case class SimpleAbbrevForwardStep(nodeTest: NodeTest) extends AbbrevForwardStep:

  def children: IndexedSeq[XPathElem] = IndexedSeq(nodeTest)

final case class AttributeAxisAbbrevForwardStep(nodeTest: NodeTest) extends AbbrevForwardStep:

  def children: IndexedSeq[XPathElem] = IndexedSeq(nodeTest)

sealed trait ReverseStep extends XPathElem

final case class NonAbbrevReverseStep(reverseAxis: ReverseAxis, nodeTest: NodeTest) extends ReverseStep:

  def children: IndexedSeq[XPathElem] = IndexedSeq(reverseAxis, nodeTest)

case object AbbrevReverseStep extends ReverseStep:

  def children: IndexedSeq[XPathElem] = IndexedSeq()

sealed trait NodeTest extends XPathElem

sealed trait KindTest extends NodeTest

sealed trait NameTest extends NodeTest

final case class SimpleNameTest(name: EQName) extends NameTest with LeafElem

sealed trait Wildcard extends NameTest

case object AnyWildcard extends Wildcard with LeafElem

final case class PrefixWildcard(prefix: NCName) extends Wildcard with LeafElem

final case class LocalNameWildcard(localName: NCName) extends Wildcard with LeafElem

final case class NamespaceWildcard(bracedUriLiteral: BracedUriLiteral) extends Wildcard with LeafElem

sealed trait DocumentTest extends KindTest

case object SimpleDocumentTest extends DocumentTest with LeafElem

final case class DocumentTestContainingElementTest(elementTest: ElementTest) extends DocumentTest:

  def children: IndexedSeq[XPathElem] = IndexedSeq(elementTest)

final case class DocumentTestContainingSchemaElementTest(schemaElementTest: SchemaElementTest) extends DocumentTest:

  def children: IndexedSeq[XPathElem] = IndexedSeq(schemaElementTest)

sealed trait ElementTest extends KindTest

case object AnyElementTest extends ElementTest with LeafElem

final case class ElementNameTest(name: EQName) extends ElementTest with LeafElem

final case class ElementNameAndTypeTest(name: EQName, tpe: EQName) extends ElementTest with LeafElem

final case class NillableElementNameAndTypeTest(name: EQName, tpe: EQName) extends ElementTest with LeafElem

final case class ElementTypeTest(tpe: EQName) extends ElementTest with LeafElem

final case class NillableElementTypeTest(tpe: EQName) extends ElementTest with LeafElem

sealed trait AttributeTest extends KindTest

case object AnyAttributeTest extends AttributeTest with LeafElem

final case class AttributeNameTest(name: EQName) extends AttributeTest with LeafElem

final case class AttributeNameAndTypeTest(name: EQName, tpe: EQName) extends AttributeTest with LeafElem

final case class AttributeTypeTest(tpe: EQName) extends AttributeTest with LeafElem

final case class SchemaElementTest(name: EQName) extends KindTest with LeafElem

final case class SchemaAttributeTest(name: EQName) extends KindTest with LeafElem

sealed trait PITest extends KindTest

case object SimplePITest extends PITest with LeafElem

// TODO Is this correct?
final case class TargetPITest(target: NCName) extends PITest with LeafElem

// TODO Is this correct?
final case class DataPITest(data: StringLiteral) extends PITest:

  def children: IndexedSeq[XPathElem] = IndexedSeq(data)

case object CommentTest extends KindTest with LeafElem

case object TextTest extends KindTest with LeafElem

case object NamespaceNodeTest extends KindTest with LeafElem

case object AnyKindTest extends KindTest with LeafElem

// Primary expressions

/**
 * Primary expression, which are the basic primitives of the language. Examples are literals,
 * variable references, function calls etc. Note that primary expressions can be rather simple but they
 * do not have to be simple. For example, function calls can have arguments that are themselves quite
 * complex expressions.
 */
sealed trait PrimaryExpr extends SimplePostfixExpr

sealed trait Literal extends PrimaryExpr

final case class StringLiteral(value: String) extends Literal with LeafElem

sealed trait NumericLiteral extends Literal

final case class IntegerLiteral(value: BigInt) extends NumericLiteral with LeafElem

final case class DecimalLiteral(value: BigDecimal) extends NumericLiteral with LeafElem

final case class DoubleLiteral(value: Double) extends NumericLiteral with LeafElem

final case class VarRef(varName: EQName) extends PrimaryExpr with LeafElem

final case class ParenthesizedExpr(exprOption: Option[Expr]) extends PrimaryExpr:

  def children: IndexedSeq[XPathElem] = exprOption.toIndexedSeq

case object ContextItemExpr extends PrimaryExpr with LeafElem

final case class FunctionCall(functionName: EQName, argumentList: ArgumentList) extends PrimaryExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(argumentList)

sealed trait FunctionItemExpr extends PrimaryExpr

final case class NamedFunctionRef(functionName: EQName, arity: BigInt) extends FunctionItemExpr with LeafElem

final case class InlineFunctionExpr(
    paramListOption: Option[ParamList],
    resultTypeOption: Option[SequenceType],
    body: EnclosedExpr)
    extends FunctionItemExpr:

  def children: IndexedSeq[XPathElem] =
    paramListOption.toIndexedSeq.appendedAll(resultTypeOption.toIndexedSeq).appended(body)

final case class MapConstructor(entries: IndexedSeq[MapConstructorEntry]) extends PrimaryExpr:

  def children: IndexedSeq[XPathElem] = entries

sealed trait ArrayConstructor extends PrimaryExpr

final case class UnaryLookup(keySpecifier: KeySpecifier) extends PrimaryExpr:

  def children: IndexedSeq[XPathElem] = IndexedSeq(keySpecifier)

final case class SquareArrayConstructor(members: IndexedSeq[ExprSingle]) extends ArrayConstructor:

  def children: IndexedSeq[XPathElem] = members

final case class CurlyArrayConstructor(expr: EnclosedExpr) extends ArrayConstructor:

  def children: IndexedSeq[XPathElem] = IndexedSeq(expr)

final case class MapConstructorEntry(keyExpr: ExprSingle, valueExpr: ExprSingle) extends XPathElem:

  def children: IndexedSeq[XPathElem] = IndexedSeq(keyExpr, valueExpr)

sealed trait Postfix extends XPathElem

final case class Predicate(expr: Expr) extends Postfix:

  def children: IndexedSeq[XPathElem] = IndexedSeq(expr)

final case class ArgumentList(arguments: IndexedSeq[Argument]) extends Postfix:

  def children: IndexedSeq[XPathElem] = arguments

final case class PostfixLookup(keySpecifier: KeySpecifier) extends Postfix:

  def children: IndexedSeq[XPathElem] = IndexedSeq(keySpecifier)

sealed trait KeySpecifier extends XPathElem

final case class NamedKeySpecifier(ncName: NCName) extends KeySpecifier with LeafElem

final case class PositionalKeySpecifier(integerLiteral: IntegerLiteral) extends KeySpecifier:

  def children: IndexedSeq[XPathElem] = IndexedSeq(integerLiteral)

case object WildcardKeySpecifier extends KeySpecifier with LeafElem

final case class ParenthesizedExprKeySpecifier(parenthesizedExpr: ParenthesizedExpr) extends KeySpecifier:

  def children: IndexedSeq[XPathElem] = IndexedSeq(parenthesizedExpr)

final case class ParamList(params: IndexedSeq[Param]) extends XPathElem:

  def children: IndexedSeq[XPathElem] = params

final case class Param(paramName: EQName, typeDeclarationOption: Option[TypeDeclaration]) extends XPathElem:

  def children: IndexedSeq[XPathElem] = typeDeclarationOption.toIndexedSeq

sealed trait Argument extends XPathElem

final case class ExprSingleArgument(exprSingle: ExprSingle) extends Argument:

  def children: IndexedSeq[XPathElem] = IndexedSeq(exprSingle)

case object ArgumentPlaceholder extends Argument with LeafElem

// Bindings

/**
 * Binding of a variable name to an expression.
 */
sealed trait VariableBinding extends XPathElem:

  def varName: EQName

  def expr: ExprSingle

final case class SimpleForBinding(varName: EQName, expr: ExprSingle) extends VariableBinding:

  def children: IndexedSeq[XPathElem] = IndexedSeq(expr)

final case class SimpleLetBinding(varName: EQName, expr: ExprSingle) extends VariableBinding:

  def children: IndexedSeq[XPathElem] = IndexedSeq(expr)

final case class SimpleBindingInQuantifiedExpr(varName: EQName, expr: ExprSingle) extends VariableBinding:

  def children: IndexedSeq[XPathElem] = IndexedSeq(expr)

// Types

sealed trait SequenceType extends XPathElem

case object EmptySequenceType extends SequenceType with LeafElem

final case class ExactlyOneSequenceType(itemType: ItemType) extends SequenceType:

  def children: IndexedSeq[XPathElem] = IndexedSeq(itemType)

final case class ZeroOrOneSequenceType(itemType: ItemType) extends SequenceType:

  def children: IndexedSeq[XPathElem] = IndexedSeq(itemType)

final case class ZeroOrMoreSequenceType(itemType: ItemType) extends SequenceType:

  def children: IndexedSeq[XPathElem] = IndexedSeq(itemType)

final case class OneOrMoreSequenceType(itemType: ItemType) extends SequenceType:

  def children: IndexedSeq[XPathElem] = IndexedSeq(itemType)

sealed trait SingleType extends XPathElem

final case class NonEmptySingleType(name: EQName) extends SingleType with LeafElem

final case class PotentiallyEmptySingleType(name: EQName) extends SingleType with LeafElem

sealed trait ItemType extends XPathElem

final case class KindTestItemType(kindTest: KindTest) extends ItemType:

  def children: IndexedSeq[XPathElem] = IndexedSeq(kindTest)

case object AnyItemType extends ItemType with LeafElem

sealed trait FunctionTest extends ItemType

case object AnyFunctionTest extends FunctionTest with LeafElem

final case class TypedFunctionTest(argumentTypes: IndexedSeq[SequenceType], resultType: SequenceType)
    extends FunctionTest:

  def children: IndexedSeq[XPathElem] = argumentTypes.appended(resultType)

final case class AtomicOrUnionType(tpe: EQName) extends ItemType with LeafElem

final case class ParenthesizedItemType(itemType: ItemType) extends ItemType:

  def children: IndexedSeq[XPathElem] = IndexedSeq(itemType)

sealed trait MapTest extends ItemType

case object AnyMapTest extends MapTest with LeafElem

final case class TypedMapTest(keyType: AtomicOrUnionType, valueType: SequenceType) extends MapTest:

  def children: IndexedSeq[XPathElem] = IndexedSeq(keyType, valueType)

sealed trait ArrayTest extends ItemType

case object AnyArrayTest extends ArrayTest with LeafElem

final case class TypedArrayTest(elementType: SequenceType) extends ArrayTest:

  def children: IndexedSeq[XPathElem] = IndexedSeq(elementType)

final case class TypeDeclaration(tpe: SequenceType) extends XPathElem:

  def children: IndexedSeq[XPathElem] = IndexedSeq(tpe)

// Axes

sealed trait ForwardAxis extends XPathElem with LeafElem

object ForwardAxis:

  case object Child extends ForwardAxis:
    override def toString: String = "child"

  case object Descendant extends ForwardAxis:
    override def toString: String = "descendant"

  case object Attribute extends ForwardAxis:
    override def toString: String = "attribute"

  case object Self extends ForwardAxis:
    override def toString: String = "self"

  case object DescendantOrSelf extends ForwardAxis:
    override def toString: String = "descendant-or-self"

  case object FollowingSibling extends ForwardAxis:
    override def toString: String = "following-sibling"

  case object Following extends ForwardAxis:
    override def toString: String = "following"

  case object Namespace extends ForwardAxis:
    override def toString: String = "namespace"

  def parse(s: String): ForwardAxis = s.trim match
    case "child"              => Child
    case "descendant"         => Descendant
    case "attribute"          => Attribute
    case "self"               => Self
    case "descendant-or-self" => DescendantOrSelf
    case "following-sibling"  => FollowingSibling
    case "following"          => Following
    case "namespace"          => Namespace

sealed trait ReverseAxis extends XPathElem with LeafElem

object ReverseAxis:

  case object Parent extends ReverseAxis:
    override def toString: String = "parent"

  case object Ancestor extends ReverseAxis:
    override def toString: String = "ancestor"

  case object PrecedingSibling extends ReverseAxis:
    override def toString: String = "preceding-sibling"

  case object Preceding extends ReverseAxis:
    override def toString: String = "preceding"

  case object AncestorOrSelf extends ReverseAxis:
    override def toString: String = "ancestor-or-self"

  def parse(s: String): ReverseAxis = s.trim match
    case "parent"            => Parent
    case "ancestor"          => Ancestor
    case "preceding-sibling" => PrecedingSibling
    case "preceding"         => Preceding
    case "ancestor-or-self"  => AncestorOrSelf

// Operators

sealed trait Comp extends XPathElem with LeafElem

sealed trait ValueComp extends Comp

sealed trait GeneralComp extends Comp

sealed trait NodeComp extends Comp

object ValueComp:

  case object Eq extends ValueComp:
    override def toString: String = "eq"

  case object Ne extends ValueComp:
    override def toString: String = "ne"

  case object Lt extends ValueComp:
    override def toString: String = "lt"

  case object Le extends ValueComp:
    override def toString: String = "le"

  case object Gt extends ValueComp:
    override def toString: String = "gt"

  case object Ge extends ValueComp:
    override def toString: String = "ge"

  def parse(s: String): ValueComp = s.trim match
    case "eq" => Eq
    case "ne" => Ne
    case "lt" => Lt
    case "le" => Le
    case "gt" => Gt
    case "ge" => Ge

object GeneralComp:

  case object Eq extends GeneralComp:
    override def toString: String = "="

  case object Ne extends GeneralComp:
    override def toString: String = "!="

  case object Lt extends GeneralComp:
    override def toString: String = "<"

  case object Le extends GeneralComp:
    override def toString: String = "<="

  case object Gt extends GeneralComp:
    override def toString: String = ">"

  case object Ge extends GeneralComp:
    override def toString: String = ">="

  def parse(s: String): GeneralComp = s.trim match
    case "="  => Eq
    case "!=" => Ne
    case "<"  => Lt
    case "<=" => Le
    case ">"  => Gt
    case ">=" => Ge

object NodeComp:

  case object Is extends NodeComp:
    override def toString: String = "is"

  case object Precedes extends NodeComp:
    override def toString: String = "<<"

  case object Follows extends NodeComp:
    override def toString: String = ">>"

  def parse(s: String): NodeComp = s.trim match
    case "is" => Is
    case "<<" => Precedes
    case ">>" => Follows

sealed trait AdditionOp extends XPathElem with LeafElem

object AdditionOp:

  case object Plus extends AdditionOp:
    override def toString: String = "+"

  case object Minus extends AdditionOp:
    override def toString: String = "-"

  def parse(s: String): AdditionOp = s.trim match
    case "+" => Plus
    case "-" => Minus

sealed trait MultiplicativeOp extends XPathElem with LeafElem

object MultiplicativeOp:

  case object Times extends MultiplicativeOp:
    override def toString: String = "*"

  case object Div extends MultiplicativeOp:
    override def toString: String = "div"

  case object IDiv extends MultiplicativeOp:
    override def toString: String = "idiv"

  case object Mod extends MultiplicativeOp:
    override def toString: String = "mod"

  def parse(s: String): MultiplicativeOp = s.trim match
    case "*"    => Times
    case "div"  => Div
    case "idiv" => IDiv
    case "mod"  => Mod

sealed trait IntersectExceptOp extends XPathElem with LeafElem

object IntersectExceptOp:

  case object Intersect extends IntersectExceptOp:
    override def toString: String = "intersect"

  case object Except extends IntersectExceptOp:
    override def toString: String = "except"

  def parse(s: String): IntersectExceptOp = s.trim match
    case "intersect" => Intersect
    case "except"    => Except

sealed trait UnaryOp extends XPathElem with LeafElem

object UnaryOp:

  case object Plus extends UnaryOp:
    override def toString: String = "+"

  case object Minus extends UnaryOp:
    override def toString: String = "-"

  def parse(s: String): UnaryOp = s.trim match
    case "+" => Plus
    case "-" => Minus

sealed trait StepOp extends XPathElem with LeafElem

object StepOp:

  case object SingleSlash extends StepOp:
    override def toString: String = "/"

  case object DoubleSlash extends StepOp:
    override def toString: String = "//"

  def parse(s: String): StepOp = s.trim match
    case "/"  => SingleSlash
    case "//" => DoubleSlash

// "Keywords" etc.

sealed trait Quantifier extends XPathElem with LeafElem

object Quantifier:

  case object SomeQuantifier extends Quantifier:
    override def toString: String = "some"

  case object EveryQuantifier extends Quantifier:
    override def toString: String = "every"

  def parse(s: String): Quantifier = s.trim match
    case "some"  => SomeQuantifier
    case "every" => EveryQuantifier
