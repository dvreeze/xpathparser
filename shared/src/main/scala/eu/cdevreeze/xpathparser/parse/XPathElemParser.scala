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

import cats.data.NonEmptyVector
import eu.cdevreeze.xpathparser.ast._
import cats.parse.{Parser => P}

/**
 * XPath 3.1 AST element parsing support, using cats-parse.
 *
 * There are parsers for many kinds of XPath AST elements. These parsers typically
 * expect no leading whitespace, and they typically consume only part of the input string.
 *
 * Example of usage:
 * {{{
 * XPathElemParser.expr.parse(xpathString)
 * }}}
 *
 * Using the parsers in XPathElemParser may be somewhat risky in that they may "malfunction" when called in isolation,
 * due to the lack of context (such as cuts to avoid backtracking). Usually it is safer to stick to using the
 * XPathParser.xpathExpr parser. On the other hand, exposing parsers for specific AST elements makes it easier to
 * "decorate" specific parsers.
 *
 * @author Chris de Vreeze
 */
object XPathElemParser {

  // TODO Improve, improve, improve. Study XPath spec more closely, use cats-parse in a better way.
  // TODO Also make code complete and more robust, (slightly) improve the AST class hierarchy, etc.

  import Whitespace._

  private val DT = DelimitingTerminals
  private val NDT = NonDelimitingTerminals

  // TODO Rethink whitespace (multi/single-line, comments etc.)

  // Note that with deferred evaluation, we can define the parsers as values instead of functions.

  val expr: P[Expr] = P.defer {
    exprSingle.skipWS.repSep(min = 1, sep = DT.comma.skipWS).map { exprs =>
      Expr(NonEmptyVector.fromVectorUnsafe(exprs.toList.toVector))
    }
  }

  val enclosedExpr: P[EnclosedExpr] = P.defer {
    ((DT.openBrace.skipWS.soft *> expr.skipWS.?).soft <* DT.closeBrace.skipWS).map { expOpt =>
      EnclosedExpr(expOpt)
    }
  }

  // The branches of exprSingle are easy to distinguish. All but one start with a different keyword.
  // Anything else must be an orExpr (if parsing succeeds).

  val exprSingle: P[ExprSingle] = P.defer {
    P.oneOf(forExpr :: letExpr :: quantifiedExpr :: ifExpr :: orExpr :: Nil)
  }

  val forExpr: P[ForExpr] = P.defer {
    (((NDT.forWord.skipWS ~ simpleForBinding.skipWS
      .repSep(min = 1, sep = DT.comma.skipWS)) <* NDT.returnWord.skipWS) ~ exprSingle.skipWS).map {
      case ((_, bindings), returnExp) => ForExpr(NonEmptyVector.fromVectorUnsafe(bindings.toList.toVector), returnExp)
    }
  }

  val simpleForBinding: P[SimpleForBinding] = P.defer {
    ((DT.dollar.skipWS *> eqName.skipWS <* NDT.inWord.skipWS) ~ exprSingle.skipWS).map {
      case (eqn, exp) => SimpleForBinding(eqn, exp)
    }
  }

  val letExpr: P[LetExpr] = P.defer {
    ((NDT.letWord.skipWS *> simpleLetBinding.skipWS
      .repSep(min = 1, sep = DT.comma.skipWS) <* NDT.returnWord.skipWS) ~ exprSingle.skipWS).map {
      case (bindings, returnExp) => LetExpr(NonEmptyVector.fromVectorUnsafe(bindings.toList.toVector), returnExp)
    }
  }

  val simpleLetBinding: P[SimpleLetBinding] = P.defer {
    ((DT.dollar.skipWS *> eqName.skipWS <* DT.assignmentSymbol.skipWS) ~ exprSingle.skipWS).map {
      case (eqn, exp) => SimpleLetBinding(eqn, exp)
    }
  }

  val quantifiedExpr: P[QuantifiedExpr] = P.defer {
    (quantifier.skipWS ~ simpleBindingInQuantifiedExpr.skipWS
      .repSep(min = 1, sep = DT.comma.skipWS) ~ NDT.satisfiesWord.skipWS ~ exprSingle.skipWS)
      .map {
        case (((quant, bindings), _), satisfiesExp) =>
          QuantifiedExpr(quant, NonEmptyVector.fromVectorUnsafe(bindings.toList.toVector), satisfiesExp)
      }
  }

  val quantifier: P[Quantifier] =
    P.defer(NDT.someWord.skipWS | NDT.everyWord.skipWS).string.map(s => Quantifier.parse(s))

  val simpleBindingInQuantifiedExpr: P[SimpleBindingInQuantifiedExpr] = P.defer {
    ((DT.dollar.skipWS *> eqName.skipWS <* NDT.inWord.skipWS) ~ exprSingle.skipWS).map {
      case (eqn, exp) => SimpleBindingInQuantifiedExpr(eqn, exp)
    }
  }

  val ifExpr: P[IfExpr] = P.defer {
    (((NDT.ifWord.skipWS ~ DT.openParenthesis.skipWS) *> expr.skipWS <* DT.closeParenthesis.skipWS) ~ (NDT.thenWord.skipWS *> exprSingle.skipWS <* NDT.elseWord.skipWS) ~ exprSingle.skipWS)
      .map {
        case ((e1, e2), e3) => IfExpr(e1, e2, e3)
      }
  }

  val orExpr: P[OrExpr] = P.defer {
    (andExpr.skipWS.soft ~ (NDT.orWord.skipWS *> andExpr.skipWS).rep0).map {
      case (firstExpr, remainingExps) =>
        OrExpr(NonEmptyVector.fromVectorUnsafe(remainingExps.toVector.prepended(firstExpr)))
    }
  }

  val andExpr: P[AndExpr] = P.defer {
    (comparisonExpr.skipWS.soft ~ (NDT.andWord.skipWS *> comparisonExpr.skipWS).rep0).map {
      case (firstExpr, remainingExps) =>
        AndExpr(NonEmptyVector.fromVectorUnsafe(remainingExps.toVector.prepended(firstExpr)))
    }
  }

  val comparisonExpr: P[ComparisonExpr] = P.defer {
    (stringConcatExpr.skipWS.soft ~ (comp.skipWS ~ stringConcatExpr.skipWS).?).map {
      case (expr1, Some((op, expr2))) => CompoundComparisonExpr(expr1, op, expr2)
      case (expr, None)               => expr
    }
  }

  val stringConcatExpr: P[StringConcatExpr] = P.defer {
    (rangeExpr.skipWS.soft ~ (DT.doubleVerticalBar.skipWS *> rangeExpr.skipWS).rep0).map {
      case (firstExpr, remainingExprs) =>
        StringConcatExpr(NonEmptyVector.fromVectorUnsafe(remainingExprs.toVector.prepended(firstExpr)))
    }
  }

  val rangeExpr: P[RangeExpr] = P.defer {
    (additiveExpr.skipWS.soft ~ (NDT.toWord.skipWS *> additiveExpr.skipWS).?).map {
      case (additiveExp1, Some(additiveExp2)) => CompoundRangeExpr(additiveExp1, additiveExp2)
      case (additiveExp, None)                => additiveExp
    }
  }

  val additiveExpr: P[AdditiveExpr] = P.defer {
    (multiplicativeExpr.skipWS.soft ~ ((DT.plus | DT.minus).skipWS.string ~ multiplicativeExpr.skipWS).rep0).map {
      case (firstExp, opExpPairs) =>
        AdditiveExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => AdditionOp.parse(kv._1) -> kv._2))
    }
  }

  val multiplicativeExpr: P[MultiplicativeExpr] = P.defer {
    (unionExpr.skipWS.soft ~ ((DT.asterisk | (NDT.divWord | NDT.idivWord | NDT.modWord)).skipWS.string ~ unionExpr.skipWS).rep0)
      .map {
        case (firstExp, opExpPairs) =>
          MultiplicativeExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => MultiplicativeOp.parse(kv._1) -> kv._2))
      }
  }

  val unionExpr: P[UnionExpr] = P.defer {
    (intersectExceptExpr.skipWS.soft ~ ((NDT.unionWord | DT.verticalBar).skipWS *> intersectExceptExpr.skipWS).rep0)
      .map {
        case (expr, exprSeq) => UnionExpr(NonEmptyVector.fromVectorUnsafe(exprSeq.toVector.prepended(expr)))
      }
  }

  val intersectExceptExpr: P[IntersectExceptExpr] = P.defer {
    (instanceOfExpr.skipWS.soft ~ ((NDT.intersectWord | NDT.exceptWord).skipWS.string ~ instanceOfExpr.skipWS).rep0)
      .map {
        case (firstExp, opExpPairs) =>
          IntersectExceptExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => IntersectExceptOp.parse(kv._1) -> kv._2))
      }
  }

  val instanceOfExpr: P[InstanceOfExpr] = P.defer {
    (treatExpr.skipWS.soft ~ ((NDT.instanceWord.skipWS.soft ~ NDT.ofWord.skipWS) *> sequenceType.skipWS).?).map {
      case (expr, tpeOption) => InstanceOfExpr(expr, tpeOption)
    }
  }

  val treatExpr: P[TreatExpr] = P.defer {
    (castableExpr.skipWS.soft ~ ((NDT.treatWord.skipWS.soft ~ NDT.asWord.skipWS) *> sequenceType.skipWS).?).map {
      case (expr, tpeOption) => TreatExpr(expr, tpeOption)
    }
  }

  val castableExpr: P[CastableExpr] = P.defer {
    (castExpr.skipWS.soft ~ ((NDT.castableWord.skipWS.soft ~ NDT.asWord.skipWS) *> singleType.skipWS).?).map {
      case (expr, tpeOption) => CastableExpr(expr, tpeOption)
    }
  }

  val castExpr: P[CastExpr] = P.defer {
    (arrowExpr.skipWS.soft ~ ((NDT.castWord.skipWS.soft ~ NDT.asWord.skipWS) *> singleType.skipWS).?).map {
      case (expr, tpeOption) => CastExpr(expr, tpeOption)
    }
  }

  val arrowExpr: P[ArrowExpr] = P.defer {
    (unaryExpr.skipWS.soft ~ arrowFunctionCall.skipWS.rep0).map {
      case (exp, funCalls) => ArrowExpr(exp, funCalls.toIndexedSeq)
    }
  }

  val arrowFunctionCall: P[ArrowFunctionCall] = P.defer {
    (DT.doubleArrow.skipWS *> arrowFunctionSpecifier.skipWS ~ argumentList.skipWS).map {
      case (funcSpec, args) => ArrowFunctionCall(funcSpec, args)
    }
  }

  val arrowFunctionSpecifier: P[ArrowFunctionSpecifier] = P.defer {
    (eqName | varRef | parenthesizedExpr).map {
      case nm: EQName                 => EQNameAsArrowFunctionSpecifier(nm)
      case ref @ VarRef(_)            => VarRefAsArrowFunctionSpecifier(ref)
      case exp @ ParenthesizedExpr(_) => ParenthesizedExprAsArrowFunctionSpecifier(exp)
    }
  }

  val unaryExpr: P[UnaryExpr] = P.defer {
    ((DT.minus | DT.plus).skipWS.string.rep0.soft.with1 ~ valueExpr.skipWS).map {
      case (ops, expr) => UnaryExpr(ops.toIndexedSeq.map(op => UnaryOp.parse(op)), expr)
    }
  }

  val valueExpr: P[ValueExpr] = P.defer(simpleMapExpr)

  val simpleMapExpr: P[SimpleMapExpr] = P.defer {
    pathExpr.skipWS.repSep(min = 1, sep = DT.exclamationMark.skipWS).map { exps =>
      SimpleMapExpr(NonEmptyVector.fromVectorUnsafe(exps.toList.toVector))
    }
  }

  // According to constraint xgc:leading-lone-slash, we need to look ahead just one token to determine if a slash is a path
  // expression or if it has to be taken together with the relative path expression that must come after the slash.
  // Note that a relativePathExpr can never start with a slash (partly because an EQName cannot start with a slash).
  // Hence the 4 branches below are easy to distinguish.

  val pathExpr: P[PathExpr] = P.defer {
    P.oneOf(
      slashOnlyPathExpr :: pathExprStartingWithSingleSlash :: pathExprStartingWithDoubleSlash :: relativePathExpr :: Nil)
  }

  // Lookahead parsers, to determine if the next token can start a relative path expression.
  // For these lookahead parsers, it is not important to distinguish branch canStartAxisStep from canStartPostfixExpr.

  private val canStartRelativePathExpr: P[Unit] = P.defer(canStartAxisStep | canStartPostfixExpr)

  // The start of an axis step is easy to recognize, unless it is a nodeTest. The latter is a kindTest (easy to recognize),
  // wildcard or EQName. The EQName variant makes it harder to distinguish an axisStep from a postfixExpr.

  private val canStartAxisStep: P[Unit] = P.defer {
    P.oneOf(forwardAxis :: reverseAxis :: DT.at.skipWS :: DT.doubleDot.skipWS :: nodeTest :: Nil).void
  }

  // A postfix expression starts with a (string or numeric) literal, dollar sign, (opening) parenthesis, dot,
  // NCName or URI-qualified name or the token "function". In XPath 3.1 it can also start with token "map" or "array",
  // an open bracket, or a question mark. (Note that, like context items, decimal and double literals may start with dots.)

  private val canStartPostfixExpr: P[Unit] = P.defer {
    P.oneOf(literal :: varRef :: DT.openParenthesis.skipWS :: contextItemExpr :: eqName ::
        NDT.functionWord.skipWS :: NDT.mapWord.skipWS :: NDT.arrayWord.skipWS :: DT.openBracket.skipWS :: DT.questionMark.skipWS :: Nil)
      .void
  }

  // Looking ahead to distinguish a single slash from a double slash, and to recognize the start of a relativePathExpr.
  // See xgc:leading-lone-slash constraint.

  val slashOnlyPathExpr: P[PathExpr] = P.defer {
    (DT.slash.skipWS.soft ~ canStartRelativePathExpr.unary_!).map { _ =>
      SlashOnlyPathExpr
    }
  }

  // See above. Note that the next token is not a slash, because 2 slashes together make up one token,
  // and because canStartRelativePathExpr implies that the next token cannot be a slash anyway.

  val pathExprStartingWithSingleSlash: P[PathExpr] = P.defer {
    ((DT.slash.skipWS.soft ~ canStartRelativePathExpr.peek).soft *> relativePathExpr.skipWS).map { expr =>
      PathExprStartingWithSingleSlash(expr)
    }
  }

  val pathExprStartingWithDoubleSlash: P[PathExpr] = P.defer {
    (DT.doubleSlash.skipWS.soft *> relativePathExpr.skipWS).map { expr =>
      PathExprStartingWithDoubleSlash(expr)
    }
  }

  val relativePathExpr: P[RelativePathExpr] = P.defer {
    (stepExpr.skipWS.soft ~ ((DT.slash | DT.doubleSlash).skipWS.string ~ stepExpr.skipWS).rep0).map {
      case (firstExp, opExpPairs) =>
        RelativePathExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => StepOp.parse(kv._1) -> kv._2))
    }
  }

  // The 2 branches of a stepExpr are relatively easy to distinguish. Note that both branches may start with an EQName (or "keyword"), and other than that
  // only start with mutually exclusive tokens. The difference between the 2 branches is that an axisStep starting with an EQName only contains the EQName,
  // whereas a postfixExpr may start but may never end with an EQName. Each postfixExpr starting with an EQName is a function call or named function
  // reference. Two constraints (xgc:reserved-function-names and gn:parens) further help in recognizing function calls and named function references.

  // Hence, we first try the branch for postfixExpr, and try branch axisStep if the first one fails.

  val stepExpr: P[StepExpr] = P.defer(postfixExpr | axisStep)

  // The 2 branches of an axisStep are relatively easy to distinguish. A reverseAxisStep is easy to recognize.
  // A forwardAxisStep is easy to recognize if non-abbreviated, and otherwise it starts with a nodeTest, possibly
  // preceded by "@".

  // We first try the reverseAxisStep, and only then the forwardAxisStep, to make sure that nodeTests are only
  // tried if all other options (like non-abbreviated steps) do not apply. Note that the lookahead needed for
  // discarding reverseAxisStep is limited (2 tokens).

  val axisStep: P[AxisStep] = P.defer(reverseAxisStep | forwardAxisStep)

  val forwardAxisStep: P[ForwardAxisStep] = P.defer {
    (forwardStep.skipWS.soft ~ predicate.skipWS.rep0).map {
      case (forwardStep, predicates) => ForwardAxisStep(forwardStep, predicates.toIndexedSeq)
    }
  }

  val reverseAxisStep: P[ReverseAxisStep] = P.defer {
    (reverseStep.skipWS.soft ~ predicate.skipWS.rep0).map {
      case (reverseStep, predicates) => ReverseAxisStep(reverseStep, predicates.toIndexedSeq)
    }
  }

  val forwardStep: P[ForwardStep] = P.defer(nonAbbrevForwardStep | abbrevForwardStep)

  val abbrevForwardStep: P[AbbrevForwardStep] = P.defer(simpleAbbrevForwardStep | attributeAxisAbbrevForwardStep)

  val simpleAbbrevForwardStep: P[SimpleAbbrevForwardStep] = P.defer {
    nodeTest.map { nodeTest =>
      SimpleAbbrevForwardStep(nodeTest)
    }
  }

  val attributeAxisAbbrevForwardStep: P[AttributeAxisAbbrevForwardStep] = P.defer {
    (DT.at.skipWS.soft *> nodeTest.skipWS).map { nodeTest =>
      AttributeAxisAbbrevForwardStep(nodeTest)
    }
  }

  val nonAbbrevForwardStep: P[NonAbbrevForwardStep] = P.defer {
    (forwardAxis.skipWS ~ nodeTest.skipWS).map {
      case (axis, nodeTest) => NonAbbrevForwardStep(axis, nodeTest)
    }
  }

  val forwardAxis: P[ForwardAxis] = P.defer {
    ((NDT.childWord | NDT.descendantWord | NDT.attributeWord | NDT.selfWord | NDT.descendantOrSelfWord |
      NDT.followingSiblingWord | NDT.followingWord | NDT.namespaceWord).skipWS.string.soft <* DT.doubleColon.skipWS)
      .map {

        case "child"              => ForwardAxis.Child
        case "descendant"         => ForwardAxis.Descendant
        case "attribute"          => ForwardAxis.Attribute
        case "self"               => ForwardAxis.Self
        case "descendant-or-self" => ForwardAxis.DescendantOrSelf
        case "following-sibling"  => ForwardAxis.FollowingSibling
        case "following"          => ForwardAxis.Following
        case "namespace"          => ForwardAxis.Namespace
      }
  }

  val reverseStep: P[ReverseStep] = P.defer(nonAbbrevReverseStep | abbrevReverseStep)

  val abbrevReverseStep: P[AbbrevReverseStep.type] =
    P.defer(DT.doubleDot.skipWS.as(AbbrevReverseStep))

  val nonAbbrevReverseStep: P[NonAbbrevReverseStep] = P.defer {
    (reverseAxis.skipWS ~ nodeTest.skipWS).map {
      case (axis, nodeTest) => NonAbbrevReverseStep(axis, nodeTest)
    }
  }

  val reverseAxis: P[ReverseAxis] = P.defer {
    ((NDT.parentWord | NDT.ancestorWord | NDT.precedingSiblingWord | NDT.precedingWord | NDT.ancestorOrSelfWord).skipWS.string.soft <* DT.doubleColon.skipWS)
      .map {
        case "parent"            => ReverseAxis.Parent
        case "ancestor"          => ReverseAxis.Ancestor
        case "preceding-sibling" => ReverseAxis.PrecedingSibling
        case "preceding"         => ReverseAxis.Preceding
        case "ancestor-or-self"  => ReverseAxis.AncestorOrSelf
      }
  }

  // The 2 branches of a nodeTest are easy to distinguish, with limited lookahead.
  // We first try branch kindTest, which always starts with a "keyword". If that fails, we try the nameTest branch.

  val nodeTest: P[NodeTest] = P.defer(kindTest | nameTest)

  // The 2 branches of a nameTest are relatively easy to distinguish. A simpleNameTest is just an EQName, whereas a wildcard
  // always contains an asterisk. Also mind the remarks below.

  // As per the grammar specification, we first try the simpleNameTest branch, and then, if it fails, the wildcard branch.
  // This way a URI qualified name will be recognized before the "braced URI literal wildcard". Because of the ws:explicit
  // constraint a prefix wildcard should not be "hidden" by a QName as EQName, but to make sure a prefix
  // wildcard is recognized we look ahead.

  val nameTest: P[NameTest] = P.defer(simpleNameTest | wildcard)

  val simpleNameTest: P[SimpleNameTest] = P.defer {
    (eqName.skipWS.soft <* DT.colonAsterisk.skipWS.unary_!).map { name =>
      SimpleNameTest(name)
    }
  }

  // See ws:explicit constraint.

  val wildcard: P[Wildcard] = P.defer(Wildcards.wildcard.skipWS)

  val kindTest: P[KindTest] =
    P.defer {
      documentTest | elementTest | attributeTest | schemaElementTest | schemaAttributeTest | piTest | commentTest | textTest | namespaceNodeTest | anyKindTest
    }

  val documentTest: P[DocumentTest] =
    P.defer(simpleDocumentTest | documentTestContainingElementTest | documentTestContainingSchemaElementTest)

  val simpleDocumentTest: P[SimpleDocumentTest.type] = P.defer {
    ((NDT.documentNodeWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.closeParenthesis.skipWS).map(_ =>
      SimpleDocumentTest)
  }

  val documentTestContainingElementTest: P[DocumentTestContainingElementTest] = P.defer {
    (((NDT.documentNodeWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> elementTest.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map { elemTest =>
        DocumentTestContainingElementTest(elemTest)
      }
  }

  val documentTestContainingSchemaElementTest: P[DocumentTestContainingSchemaElementTest] = P.defer {
    (((NDT.documentNodeWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> schemaElementTest.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map { schemaElmTest =>
        DocumentTestContainingSchemaElementTest(schemaElmTest)
      }
  }

  val elementTest: P[ElementTest] = P.defer {
    (anyElementTest | elementNameTest | elementNameAndTypeTest | nillableElementNameAndTypeTest | elementTypeTest | nillableElementTypeTest)
  }

  // Losing some efficiency on parsing of element tests

  val anyElementTest: P[AnyElementTest.type] = P.defer {
    (((NDT.elementWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.asterisk.skipWS.?).soft ~ DT.closeParenthesis.skipWS)
      .as(AnyElementTest)
  }

  val elementNameTest: P[ElementNameTest] = P.defer {
    (((NDT.elementWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> eqName.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map { name =>
        ElementNameTest(name)
      }
  }

  val elementNameAndTypeTest: P[ElementNameAndTypeTest] = P.defer {
    (((((NDT.elementWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> eqName.skipWS).soft <* DT.comma.skipWS) ~ eqName.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map {
        case (name, tpe) => ElementNameAndTypeTest(name, tpe)
      }
  }

  val nillableElementNameAndTypeTest: P[NillableElementNameAndTypeTest] = P.defer {
    ((((((NDT.elementWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> eqName.skipWS).soft <* DT.comma.skipWS)).soft ~ eqName.skipWS).soft <* (DT.questionMark.skipWS.soft ~ DT.closeParenthesis.skipWS))
      .map {
        case (name, tpe) => NillableElementNameAndTypeTest(name, tpe)
      }
  }

  val elementTypeTest: P[ElementTypeTest] = P.defer {
    (((((NDT.elementWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.asterisk.skipWS).soft ~ DT.comma.skipWS).soft *> eqName.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map { tpe =>
        ElementTypeTest(tpe)
      }
  }

  val nillableElementTypeTest: P[NillableElementTypeTest] = P.defer {
    (((((NDT.elementWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.asterisk.skipWS).soft ~ DT.comma.skipWS).soft *> eqName.skipWS).soft <* (DT.questionMark.skipWS.soft ~ DT.closeParenthesis.skipWS))
      .map { tpe =>
        NillableElementTypeTest(tpe)
      }
  }

  val attributeTest: P[AttributeTest] =
    P.defer(anyAttributeTest | attributeNameTest | attributeNameAndTypeTest | attributeTypeTest)

  // Losing some efficiency on parsing of attribute tests

  val anyAttributeTest: P[AnyAttributeTest.type] = P.defer {
    (((NDT.attributeWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.asterisk.skipWS.?).soft ~ DT.closeParenthesis.skipWS)
      .as(AnyAttributeTest)
  }

  val attributeNameTest: P[AttributeNameTest] = P.defer {
    (((NDT.attributeWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> eqName.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map { name =>
        AttributeNameTest(name)
      }
  }

  val attributeNameAndTypeTest: P[AttributeNameAndTypeTest] = P.defer {
    (((((NDT.attributeWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> eqName.skipWS).soft <* DT.comma.skipWS) ~ eqName.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map {
        case (name, tpe) => AttributeNameAndTypeTest(name, tpe)
      }
  }

  val attributeTypeTest: P[AttributeTypeTest] = P.defer {
    (((((NDT.attributeWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.asterisk.skipWS).soft ~ DT.comma.skipWS).soft *> eqName.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map { tpe =>
        AttributeTypeTest(tpe)
      }
  }

  val schemaElementTest: P[SchemaElementTest] = P.defer {
    (((NDT.schemaElementWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> eqName.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map { name =>
        SchemaElementTest(name)
      }
  }

  val schemaAttributeTest: P[SchemaAttributeTest] = P.defer {
    (((NDT.schemaAttributeWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> eqName.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map { name =>
        SchemaAttributeTest(name)
      }
  }

  val piTest: P[PITest] = P.defer(simplePiTest | targetPiTest | dataPiTest)

  val simplePiTest: P[SimplePITest.type] =
    P.defer((NDT.processingInstructionWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.closeParenthesis.skipWS)
      .map(_ => SimplePITest)

  val targetPiTest: P[TargetPITest] = P.defer {
    (((NDT.processingInstructionWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> ncName.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map { name =>
        TargetPITest(name)
      }
  }

  val dataPiTest: P[DataPITest] = P.defer {
    (((NDT.processingInstructionWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> stringLiteral.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map { stringLit =>
        DataPITest(stringLit)
      }
  }

  val commentTest: P[CommentTest.type] =
    P.defer((NDT.commentWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.closeParenthesis.skipWS).as(CommentTest)

  val textTest: P[TextTest.type] =
    P.defer((NDT.textWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.closeParenthesis.skipWS).as(TextTest)

  val namespaceNodeTest: P[NamespaceNodeTest.type] =
    P.defer((NDT.namespaceNodeWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.closeParenthesis.skipWS)
      .as(NamespaceNodeTest)

  val anyKindTest: P[AnyKindTest.type] =
    P.defer((NDT.nodeWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.closeParenthesis.skipWS).as(AnyKindTest)

  // Postfix expressions

  val postfixExpr: P[PostfixExpr] = P.defer {
    (primaryExpr.skipWS.soft ~ (predicate | argumentList | lookup).skipWS.rep0).map {
      case (primaryExp, postfixes) => PostfixExpr(primaryExp, postfixes.toIndexedSeq)
    }
  }

  val argumentList: P[ArgumentList] = P.defer {
    ((DT.openParenthesis.skipWS.soft *> argument.skipWS.repSep0(sep = DT.comma.skipWS)).soft <* DT.closeParenthesis.skipWS)
      .map { args =>
        ArgumentList(args.toIndexedSeq)
      }
  }

  val argument: P[Argument] = P.defer(argumentPlaceholder | exprSingleArgument)

  val argumentPlaceholder: P[ArgumentPlaceholder.type] =
    P.defer(DT.questionMark.skipWS.as(ArgumentPlaceholder))

  val exprSingleArgument: P[ExprSingleArgument] = P.defer {
    exprSingle.map { exp =>
      ExprSingleArgument(exp)
    }
  }

  val lookup: P[PostfixLookup] = P.defer {
    (DT.questionMark.skipWS.soft *> keySpecifier.skipWS).map { keySpec =>
      PostfixLookup(keySpec)
    }
  }

  val keySpecifier: P[KeySpecifier] = P.defer {
    (ncName.skipWS | integerLiteral.skipWS | DT.asterisk.skipWS.string | parenthesizedExpr.skipWS).map {
      case nm: NCName             => NamedKeySpecifier(nm)
      case intLit: IntegerLiteral => PositionalKeySpecifier(intLit)
      case "*"                    => WildcardKeySpecifier
      case exp: ParenthesizedExpr => ParenthesizedExprKeySpecifier(exp)
    }
  }

  // TODO Minimally one param?
  val paramList: P[ParamList] = P.defer {
    param.skipWS.repSep(min = 1, sep = DT.comma.skipWS).map { pars =>
      ParamList(pars.toList.toVector)
    }
  }

  val param: P[Param] = P.defer {
    ((DT.dollar.skipWS.soft *> eqName.skipWS).soft ~ (NDT.asWord.skipWS *> sequenceType.skipWS).?).map {
      case (name, tpeOption) => Param(name, tpeOption.map(t => TypeDeclaration(t)))
    }
  }

  val predicate: P[Predicate] = P.defer {
    ((DT.openBracket.skipWS.soft *> expr.skipWS).soft <* DT.closeBracket.skipWS).map { exp =>
      Predicate(exp)
    }
  }

  // Primary expressions

  // The branches of a primaryExpr are relatively easy to distinguish. See above.

  val primaryExpr: P[PrimaryExpr] = P.defer {
    literal | varRef | parenthesizedExpr | contextItemExpr | functionCall | functionItemExpr |
      mapConstructor | arrayConstructor | unaryLookup
  }

  val literal: P[Literal] = P.defer(stringLiteral | numericLiteral)

  // Using the StringLiterals.stringLiteral parser, etc.

  val stringLiteral: P[StringLiteral] =
    P.defer(DT.stringLiteral.skipWS)

  val numericLiteral: P[NumericLiteral] =
    P.defer(NDT.numericLiteral.skipWS)

  val integerLiteral: P[IntegerLiteral] =
    P.defer(NDT.integerLiteral.skipWS)

  val varRef: P[VarRef] =
    P.defer(DT.dollar.skipWS.soft *> eqName.skipWS).map { name =>
      VarRef(name)
    }

  val parenthesizedExpr: P[ParenthesizedExpr] = P.defer {
    ((DT.openParenthesis.skipWS.soft *> expr.skipWS.?).soft <* DT.closeParenthesis.skipWS).map { expOption =>
      ParenthesizedExpr(expOption)
    }
  }

  val contextItemExpr: P[ContextItemExpr.type] =
    P.defer(DT.dot.skipWS.as(ContextItemExpr))

  // See xgc:reserved-function-names
  // TODO gn:parens. This becomes important once we support comments.

  val functionCall: P[FunctionCall] = P.defer {
    (eqName.skipWS.filter(nm => !ReservedFunctionNames.contains(nm)).backtrack.soft ~ argumentList.skipWS).map {
      case (name, argList) => FunctionCall(name, argList)
    }
  }

  val functionItemExpr: P[FunctionItemExpr] = P.defer(namedFunctionRef | inlineFunctionExpr)

  // See xgc:reserved-function-names

  val namedFunctionRef: P[NamedFunctionRef] = P.defer {
    ((eqName.skipWS.filter(nm => !ReservedFunctionNames.contains(nm)).backtrack.soft <* DT.hash.skipWS).soft ~ integerLiteral)
      .map {
        case (name, arity) => NamedFunctionRef(name, arity.value)
      }
  }

  val inlineFunctionExpr: P[InlineFunctionExpr] = P.defer {
    ((((NDT.functionWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> paramList.skipWS.?).soft <* DT.closeParenthesis.skipWS).soft ~ (NDT.asWord.skipWS.soft *> sequenceType.skipWS).? ~ enclosedExpr.skipWS)
      .map {
        case ((parListOption, resultTpeOption), body) =>
          InlineFunctionExpr(parListOption, resultTpeOption, body)
      }
  }

  val mapConstructor: P[MapConstructor] = P.defer {
    ((NDT.mapWord.skipWS.soft ~ DT.openBrace.skipWS).soft *> mapConstructorEntry.skipWS
      .repSep0(sep = DT.comma.skipWS) <* DT.closeBrace.skipWS).map { entries =>
      MapConstructor(entries.toIndexedSeq)
    }
  }

  val mapConstructorEntry: P[MapConstructorEntry] = P.defer {
    ((exprSingle.skipWS <* DT.colon.skipWS) ~ exprSingle.skipWS).map {
      case (k, v) => MapConstructorEntry(k, v)
    }
  }

  val arrayConstructor: P[ArrayConstructor] =
    P.defer(squareArrayConstructor | curlyArrayConstructor)

  val squareArrayConstructor: P[SquareArrayConstructor] = P.defer {
    ((DT.openBracket.skipWS.soft *> exprSingle.skipWS.repSep0(sep = DT.comma.skipWS)).soft <* DT.closeBracket.skipWS).map {
      members =>
        SquareArrayConstructor(members.toIndexedSeq)
    }
  }

  val curlyArrayConstructor: P[CurlyArrayConstructor] = P.defer {
    (NDT.arrayWord.skipWS.soft *> enclosedExpr.skipWS).map { exp =>
      CurlyArrayConstructor(exp)
    }
  }

  val unaryLookup: P[UnaryLookup] = P.defer {
    (DT.questionMark.skipWS.soft *> keySpecifier.skipWS).map { keySpec =>
      UnaryLookup(keySpec)
    }
  }

  // Types

  val sequenceType: P[SequenceType] = P.defer(emptySequenceType | nonEmptySequenceType)

  val emptySequenceType: P[EmptySequenceType.type] = P.defer {
    ((NDT.emptySequenceWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.closeParenthesis.skipWS).as(EmptySequenceType)
  }

  // TODO xgc:occurrence-indicators

  val nonEmptySequenceType: P[SequenceType] =
    P.defer(itemType.skipWS.soft ~ (DT.questionMark | DT.asterisk | DT.plus).skipWS.string.?).map {
      case (tpe, None)      => ExactlyOneSequenceType(tpe)
      case (tpe, Some("?")) => ZeroOrOneSequenceType(tpe)
      case (tpe, Some("*")) => ZeroOrMoreSequenceType(tpe)
      case (tpe, Some("+")) => OneOrMoreSequenceType(tpe)
      case _                => EmptySequenceType
    }

  val itemType: P[ItemType] = P.defer {
    P.oneOf(
      kindTestItemType :: anyItemType :: anyFunctionTest :: typedFunctionTest :: atomicOrUnionType :: parenthesizedItemType :: mapTest :: arrayTest :: Nil)
  }

  val kindTestItemType: P[KindTestItemType] =
    P.defer(kindTest).map { kindTst =>
      KindTestItemType(kindTst)
    }

  val anyItemType: P[AnyItemType.type] = P.defer {
    ((NDT.itemWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.closeParenthesis.skipWS).as(AnyItemType)
  }

  val anyFunctionTest: P[AnyFunctionTest.type] = P.defer {
    (((NDT.functionWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.asterisk.skipWS).soft ~ DT.closeParenthesis.skipWS)
      .as(AnyFunctionTest)
  }

  val typedFunctionTest: P[TypedFunctionTest] = P.defer {
    (((NDT.functionWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> sequenceType.skipWS
      .repSep0(sep = DT.comma.skipWS) <* DT.closeParenthesis.skipWS).soft ~ (NDT.asWord.skipWS.soft *> sequenceType.skipWS)).map {
      case (parTpes, resultTpe) => TypedFunctionTest(parTpes.toIndexedSeq, resultTpe)
    }
  }

  val atomicOrUnionType: P[AtomicOrUnionType] =
    P.defer(eqName).map { tpe =>
      AtomicOrUnionType(tpe)
    }

  val parenthesizedItemType: P[ParenthesizedItemType] = P.defer {
    ((DT.openParenthesis.skipWS.soft *> itemType.skipWS).soft <* DT.closeParenthesis.skipWS).map { tpe =>
      ParenthesizedItemType(tpe)
    }
  }

  val mapTest: P[MapTest] = P.defer(anyMapTest | typedMapTest)

  val anyMapTest: P[AnyMapTest.type] = P.defer {
    (((NDT.mapWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.asterisk.skipWS).soft ~ DT.closeParenthesis.skipWS)
      .as(AnyMapTest)
  }

  val typedMapTest: P[TypedMapTest] = P.defer {
    (((((NDT.mapWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft *> atomicOrUnionType.skipWS).soft <* DT.comma.skipWS).soft ~ sequenceType.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map {
        case (kt, vt) => TypedMapTest(kt, vt)
      }
  }

  val arrayTest: P[ArrayTest] = P.defer(anyArrayTest | typedArrayTest)

  val anyArrayTest: P[AnyArrayTest.type] = P.defer {
    (((NDT.arrayWord.skipWS.soft ~ DT.openParenthesis.skipWS).soft ~ DT.asterisk.skipWS).soft ~ DT.closeParenthesis.skipWS)
      .as(AnyArrayTest)
  }

  val typedArrayTest: P[TypedArrayTest] = P.defer {
    (((NDT.arrayWord.skipWS.soft *> DT.openParenthesis.skipWS).soft *> sequenceType.skipWS).soft <* DT.closeParenthesis.skipWS)
      .map { et =>
        TypedArrayTest(et)
      }
  }

  val singleType: P[SingleType] = P.defer {
    (eqName.skipWS.soft ~ DT.questionMark.skipWS.string.?).map {
      case (tpe, None)    => NonEmptySingleType(tpe)
      case (tpe, Some(_)) => PotentiallyEmptySingleType(tpe)
    }
  }

  // Utility data/methods

  private val ReservedFunctionNames: Set[EQName] = Set(
    EQName.QName("array"),
    EQName.QName("attribute"),
    EQName.QName("comment"),
    EQName.QName("document-node"),
    EQName.QName("element"),
    EQName.QName("empty-sequence"),
    EQName.QName("function"),
    EQName.QName("if"),
    EQName.QName("item"),
    EQName.QName("map"),
    EQName.QName("namespace-node"),
    EQName.QName("node"),
    EQName.QName("processing-instruction"),
    EQName.QName("schema-attribute"),
    EQName.QName("schema-element"),
    EQName.QName("switch"),
    EQName.QName("text"),
    EQName.QName("typeswitch")
  )

  // Operators etc.

  val valueComp: P[ValueComp] = P.defer {
    (NDT.eqWord | NDT.neWord | NDT.ltWord | NDT.leWord | NDT.gtWord | NDT.geWord).skipWS.string
      .map(s => ValueComp.parse(s))
  }

  val generalComp: P[GeneralComp] = P.defer {
    (DT.equals | DT.notEquals | DT.lessThan | DT.lessThanOrEqual |
      DT.greaterThan | DT.greaterThanOrEqual).skipWS.string
      .map(s => GeneralComp.parse(s))
  }

  val nodeComp: P[NodeComp] = P.defer {
    (NDT.isWord | DT.precedes | DT.follows).skipWS.string
      .map(s => NodeComp.parse(s))
  }

  val comp: P[Comp] = P.defer(P.oneOf(valueComp :: generalComp :: nodeComp :: Nil))

  // Names (EQNames, NCNames etc.)
  // Using the NCNames.ncName and EQNames.eqName parsers

  private val ncName: P[NCName] = NCNames.ncName.skipWS

  private val eqName: P[EQName] = EQNames.eqName.skipWS
}
