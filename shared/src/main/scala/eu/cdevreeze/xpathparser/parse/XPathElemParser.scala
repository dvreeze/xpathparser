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

import eu.cdevreeze.xpathparser.ast._
import fastparse.WhitespaceApi

/**
 * XPath 3.1 AST element parsing support, using FastParse.
 *
 * There are parsers for many kinds of XPath AST elements. These parsers typically
 * expect no leading whitespace, and they typically consume only part of the input string.
 *
 * Example of usage:
 * {{{
 * XPathElemParser.expr.parse(xpathString)
 * }}}
 *
 * Using the parsers in XPathElemParser may be somewhat risky in that they malfunction when called in isolation,
 * due to the lack of context (such as cuts to avoid backtracking). Usually it is safer to stick to using the
 * XPathParser.xpathExpr parser. On the other hand, exposing parsers for specific AST elements makes it easier to
 * "decorate" specific parsers.
 *
 * TODO Make this the default implementation of a parser interface.
 *
 * @author Chris de Vreeze
 */
object XPathElemParser {

  // TODO Improve, improve, improve. Study XPath spec more closely, use FastParse in a better way,
  // make code complete and more robust, improve the AST class hierarchy, etc.

  private val DT = DelimitingTerminals
  private val NDT = NonDelimitingTerminals

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._

    // TODO Adapt. What about parsing of comments?

    NoTrace(CharPred(c => java.lang.Character.isWhitespace(c)).rep)
  }

  import White._
  import fastparse.noApi._

  val expr: P[Expr] =
    P(exprSingle.rep(min = 1, sep = DT.comma)) map {
      case (exprs) => Expr(exprs.toIndexedSeq)
    }

  val enclosedExpr: P[EnclosedExpr] =
    P(DT.openBrace ~ expr.? ~ DT.closeBrace) map {
      case expOpt => EnclosedExpr(expOpt)
    }

  // The branches of exprSingle are easy to distinguish. All but one start with a different keyword.
  // Anything else must be an orExpr (if parsing succeeds).

  val exprSingle: P[ExprSingle] =
    P(forExpr | letExpr | quantifiedExpr | ifExpr | orExpr)

  val forExpr: P[ForExpr] =
    P(NDT.forWord ~/ simpleForBinding.rep(min = 1, sep = DT.comma) ~ NDT.returnWord ~ exprSingle) map {
      case (bindings, returnExp) => ForExpr(bindings.toIndexedSeq, returnExp)
    }

  val simpleForBinding: P[SimpleForBinding] =
    P(DT.dollar ~ eqName ~ NDT.inWord ~ exprSingle) map {
      case (eqn, exp) => SimpleForBinding(eqn, exp)
    }

  val letExpr: P[LetExpr] =
    P(NDT.letWord ~/ simpleLetBinding.rep(min = 1, sep = DT.comma) ~ NDT.returnWord ~ exprSingle) map {
      case (bindings, returnExp) => LetExpr(bindings.toIndexedSeq, returnExp)
    }

  val simpleLetBinding: P[SimpleLetBinding] =
    P(DT.dollar ~ eqName ~ DT.assignmentSymbol ~ exprSingle) map {
      case (eqn, exp) => SimpleLetBinding(eqn, exp)
    }

  val quantifiedExpr: P[QuantifiedExpr] =
    P((NDT.someWord | NDT.everyWord).! ~/ simpleBindingInQuantifiedExpr.rep(min = 1, sep = DT.comma) ~ NDT.satisfiesWord ~ exprSingle) map {
      case (quant, bindings, satisfiesExp) => QuantifiedExpr(Quantifier.parse(quant), bindings.toIndexedSeq, satisfiesExp)
    }

  val simpleBindingInQuantifiedExpr: P[SimpleBindingInQuantifiedExpr] =
    P(DT.dollar ~ eqName ~ NDT.inWord ~ exprSingle) map {
      case (eqn, exp) => SimpleBindingInQuantifiedExpr(eqn, exp)
    }

  val ifExpr: P[IfExpr] =
    P(NDT.ifWord ~/ DT.openParenthesis ~ expr ~ DT.closeParenthesis ~ NDT.thenWord ~ exprSingle ~ NDT.elseWord ~ exprSingle) map {
      case (e1, e2, e3) => IfExpr(e1, e2, e3)
    }

  val orExpr: P[OrExpr] =
    P(andExpr.rep(min = 1, sep = NDT.orWord ~/ Pass)) map {
      case exps => OrExpr(exps.toIndexedSeq)
    }

  val andExpr: P[AndExpr] =
    P(comparisonExpr.rep(min = 1, sep = NDT.andWord ~/ Pass)) map {
      case exps => AndExpr(exps.toIndexedSeq)
    }

  val comparisonExpr: P[ComparisonExpr] =
    P(stringConcatExpr ~ ((valueComp | generalComp | nodeComp) ~/ stringConcatExpr).?) map {
      case (expr1, Some((op, expr2))) => CompoundComparisonExpr(expr1, op, expr2)
      case (expr, None) => expr
    }

  val stringConcatExpr: P[StringConcatExpr] =
    P(rangeExpr.rep(min = 1, sep = DT.doubleVerticalBar ~/ Pass)) map {
      case exps => StringConcatExpr(exps.toIndexedSeq)
    }

  val rangeExpr: P[RangeExpr] =
    P(additiveExpr ~ (NDT.toWord ~/ additiveExpr).?) map {
      case (additiveExp1, Some(additiveExp2)) => CompoundRangeExpr(additiveExp1, additiveExp2)
      case (additiveExp, None) => additiveExp
    }

  val additiveExpr: P[AdditiveExpr] =
    P(multiplicativeExpr ~ ((DT.plus | DT.minus).! ~/ multiplicativeExpr).rep) map {
      case (firstExp, opExpPairs) =>
        AdditiveExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => (AdditionOp.parse(kv._1) -> kv._2)))
    }

  val multiplicativeExpr: P[MultiplicativeExpr] =
    P(unionExpr ~ ((DT.asterisk | (NDT.divWord | NDT.idivWord | NDT.modWord)).! ~/ unionExpr).rep) map {
      case (firstExp, opExpPairs) =>
        MultiplicativeExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => (MultiplicativeOp.parse(kv._1) -> kv._2)))
    }

  val unionExpr: P[UnionExpr] =
    P(intersectExceptExpr ~ ((NDT.unionWord | DT.verticalBar) ~/ intersectExceptExpr).rep) map {
      case (expr, exprSeq) => UnionExpr(expr +: exprSeq.toIndexedSeq)
    }

  val intersectExceptExpr: P[IntersectExceptExpr] =
    P(instanceOfExpr ~ ((NDT.intersectWord | NDT.exceptWord).! ~/ instanceOfExpr).rep) map {
      case (firstExp, opExpPairs) =>
        IntersectExceptExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => (IntersectExceptOp.parse(kv._1) -> kv._2)))
    }

  val instanceOfExpr: P[InstanceOfExpr] =
    P(treatExpr ~ (NDT.instanceWord ~ NDT.ofWord ~/ sequenceType).?) map {
      case (expr, tpeOption) => InstanceOfExpr(expr, tpeOption)
    }

  val treatExpr: P[TreatExpr] =
    P(castableExpr ~ (NDT.treatWord ~ NDT.asWord ~/ sequenceType).?) map {
      case (expr, tpeOption) => TreatExpr(expr, tpeOption)
    }

  val castableExpr: P[CastableExpr] =
    P(castExpr ~ (NDT.castableWord ~ NDT.asWord ~/ singleType).?) map {
      case (expr, tpeOption) => CastableExpr(expr, tpeOption)
    }

  val castExpr: P[CastExpr] =
    P(arrowExpr ~ (NDT.castWord ~ NDT.asWord ~/ singleType).?) map {
      case (expr, tpeOption) => CastExpr(expr, tpeOption)
    }

  val arrowExpr: P[ArrowExpr] =
    P(unaryExpr ~ arrowFunctionCall.rep) map {
      case (exp, funCalls) => ArrowExpr(exp, funCalls.toIndexedSeq)
    }

  val arrowFunctionCall: P[ArrowFunctionCall] =
    P(DT.doubleArrow ~/ arrowFunctionSpecifier ~ argumentList) map {
      case (funcSpec, args) => ArrowFunctionCall(funcSpec, args)
    }

  val arrowFunctionSpecifier: P[ArrowFunctionSpecifier] =
    P(eqName | varRef | parenthesizedExpr) map {
      case nm: EQName => EQNameAsArrowFunctionSpecifier(nm)
      case ref @ VarRef(_) => VarRefAsArrowFunctionSpecifier(ref)
      case exp @ ParenthesizedExpr(_) => ParenthesizedExprAsArrowFunctionSpecifier(exp)
    }

  val unaryExpr: P[UnaryExpr] =
    P((DT.minus | DT.plus).!.rep ~ valueExpr) map {
      case (ops, expr) => UnaryExpr(ops.toIndexedSeq.map(op => UnaryOp.parse(op)), expr)
    }

  val valueExpr: P[ValueExpr] =
    P(simpleMapExpr)

  val simpleMapExpr: P[SimpleMapExpr] =
    P(pathExpr.rep(min = 1, sep = DT.exclamationMark)) map {
      case exps => SimpleMapExpr(exps.toIndexedSeq)
    }

  // According to constraint xgc:leading-lone-slash, we need to look ahead just one token to determine if a slash is a path
  // expression or if it has to be taken together with the relative path expression that must come after the slash.
  // Note that a relativePathExpr can never start with a slash (partly because an EQName cannot start with a slash).
  // Hence the 4 branches below are easy to distinguish.

  val pathExpr: P[PathExpr] =
    P(slashOnlyPathExpr | pathExprStartingWithSingleSlash | pathExprStartingWithDoubleSlash | relativePathExpr)

  // Lookahead parsers, to determine if the next token can start a relative path expression.
  // For these lookahead parsers, it is not important to distinguish branch canStartAxisStep from canStartPostfixExpr.

  private val canStartRelativePathExpr: P[Unit] =
    P(canStartAxisStep | canStartPostfixExpr)

  // The start of an axis step is easy to recognize, unless it is a nodeTest. The latter is a kindTest (easy to recognize),
  // wildcard or EQName. The EQName variant makes it harder to distinguish an axisStep from a postfixExpr.

  private val canStartAxisStep: P[Unit] =
    P(forwardAxis | reverseAxis | DT.at | DT.doubleDot | nodeTest).map(_ => ())

  // A postfix expression starts with a (string or numeric) literal, dollar sign, (opening) parenthesis, dot,
  // NCName or URI-qualified name or the token "function". In XPath 3.1 it can also start with token "map" or "array",
  // an open bracket, or a question mark. (Note that, like context items, decimal and double literals may start with dots.)

  private val canStartPostfixExpr: P[Unit] =
    P(literal | varRef | DT.openParenthesis | contextItemExpr | eqName |
      NDT.functionWord | NDT.mapWord | NDT.arrayWord | DT.openBracket | DT.questionMark).map(_ => ())

  // Looking ahead to distinguish a single slash from a double slash, and to recognize the start of a relativePathExpr.
  // See xgc:leading-lone-slash constraint.

  val slashOnlyPathExpr: P[PathExpr] =
    P(DT.slash ~ !canStartRelativePathExpr) map {
      case _ => SlashOnlyPathExpr
    }

  // See above. Note that the next token is not a slash, because 2 slashes together make up one token,
  // and because canStartRelativePathExpr implies that the next token cannot be a slash anyway.

  val pathExprStartingWithSingleSlash: P[PathExpr] =
    P(DT.slash ~ &(canStartRelativePathExpr) ~ relativePathExpr) map {
      case expr => PathExprStartingWithSingleSlash(expr)
    }

  val pathExprStartingWithDoubleSlash: P[PathExpr] =
    P(DT.doubleSlash ~ relativePathExpr) map {
      case expr => PathExprStartingWithDoubleSlash(expr)
    }

  val relativePathExpr: P[RelativePathExpr] =
    P(stepExpr ~ ((DT.slash | DT.doubleSlash).! ~/ stepExpr).rep) map {
      case (firstExp, opExpPairs) =>
        RelativePathExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => (StepOp.parse(kv._1) -> kv._2)))
    }

  // The 2 branches of a stepExpr are relatively easy to distinguish. Note that both branches may start with an EQName (or "keyword"), and other than that
  // only start with mutually exclusive tokens. The difference between the 2 branches is that an axisStep starting with an EQName only contains the EQName,
  // whereas a postfixExpr may start but may never end with an EQName. Each postfixExpr starting with an EQName is a function call or named function
  // reference. Two constraints (xgc:reserved-function-names and gn:parens) further help in recognizing function calls and named function references.

  // Hence, we first try the branch for postfixExpr, and try branch axisStep if the first one fails.

  val stepExpr: P[StepExpr] =
    P(postfixExpr | axisStep)

  // The 2 branches of an axisStep are relatively easy to distinguish. A reverseAxisStep is easy to recognize.
  // A forwardAxisStep is easy to recognize if non-abbreviated, and otherwise it starts with a nodeTest, possibly
  // preceded by "@".

  // We first try the reverseAxisStep, and only then the forwardAxisStep, to make sure that nodeTests are only
  // tried if all other options (like non-abbreviated steps) do not apply. Note that the lookahead needed for
  // discarding reverseAxisStep is limited (2 tokens).

  val axisStep: P[AxisStep] =
    P(reverseAxisStep | forwardAxisStep)

  val forwardAxisStep: P[ForwardAxisStep] =
    P(forwardStep ~ predicate.rep) map {
      case (forwardStep, predicates) => ForwardAxisStep(forwardStep, predicates.toIndexedSeq)
    }

  val reverseAxisStep: P[ReverseAxisStep] =
    P(reverseStep ~ predicate.rep) map {
      case (reverseStep, predicates) => ReverseAxisStep(reverseStep, predicates.toIndexedSeq)
    }

  val forwardStep: P[ForwardStep] =
    P(nonAbbrevForwardStep | abbrevForwardStep)

  val abbrevForwardStep: P[AbbrevForwardStep] =
    P(simpleAbbrevForwardStep | attributeAxisAbbrevForwardStep)

  val simpleAbbrevForwardStep: P[SimpleAbbrevForwardStep] =
    P(nodeTest) map {
      case nodeTest => SimpleAbbrevForwardStep(nodeTest)
    }

  val attributeAxisAbbrevForwardStep: P[AttributeAxisAbbrevForwardStep] =
    P(DT.at ~ nodeTest) map {
      case nodeTest => AttributeAxisAbbrevForwardStep(nodeTest)
    }

  val nonAbbrevForwardStep: P[NonAbbrevForwardStep] =
    P(forwardAxis ~/ nodeTest) map {
      case (axis, nodeTest) => NonAbbrevForwardStep(axis, nodeTest)
    }

  val forwardAxis: P[ForwardAxis] =
    P((NDT.childWord | NDT.descendantWord | NDT.attributeWord | NDT.selfWord | NDT.descendantOrSelfWord |
      NDT.followingSiblingWord | NDT.followingWord | NDT.namespaceWord).! ~ DT.doubleColon) map {

      case "child" => ForwardAxis.Child
      case "descendant" => ForwardAxis.Descendant
      case "attribute" => ForwardAxis.Attribute
      case "self" => ForwardAxis.Self
      case "descendant-or-self" => ForwardAxis.DescendantOrSelf
      case "following-sibling" => ForwardAxis.FollowingSibling
      case "following" => ForwardAxis.Following
      case "namespace" => ForwardAxis.Namespace
    }

  val reverseStep: P[ReverseStep] =
    P(nonAbbrevReverseStep | abbrevReverseStep)

  val abbrevReverseStep: P[AbbrevReverseStep.type] =
    P(DT.doubleDot) map (_ => AbbrevReverseStep)

  val nonAbbrevReverseStep: P[NonAbbrevReverseStep] =
    P(reverseAxis ~/ nodeTest) map {
      case (axis, nodeTest) => NonAbbrevReverseStep(axis, nodeTest)
    }

  val reverseAxis: P[ReverseAxis] =
    P((NDT.parentWord | NDT.ancestorWord | NDT.precedingSiblingWord | NDT.precedingWord | NDT.ancestorOrSelfWord).! ~ DT.doubleColon) map {
      case "parent" => ReverseAxis.Parent
      case "ancestor" => ReverseAxis.Ancestor
      case "preceding-sibling" => ReverseAxis.PrecedingSibling
      case "preceding" => ReverseAxis.Preceding
      case "ancestor-or-self" => ReverseAxis.AncestorOrSelf
    }

  // The 2 branches of a nodeTest are easy to distinguish, with limited lookahead.
  // We first try branch kindTest, which always starts with a "keyword". If that fails, we try the nameTest branch.

  val nodeTest: P[NodeTest] =
    P(kindTest | nameTest)

  // The 2 branches of a nameTest are relatively easy to distinguish. A simpleNameTest is just an EQName, whereas a wildcard
  // always contains an asterisk. Also mind the remarks below.

  // As per the grammar specification, we first try the simpleNameTest branch, and then, if it fails, the wildcard branch.
  // This way a URI qualified name will be recognized before the "braced URI literal wildcard". Because of the ws:explicit
  // constraint a prefix wildcard should not be "hidden" by a QName as EQName, but to make sure a prefix
  // wildcard is recognized we look ahead.

  val nameTest: P[NameTest] =
    P(simpleNameTest | wildcard)

  val simpleNameTest: P[SimpleNameTest] =
    P(eqName ~ !DT.colonAsterisk) map {
      case name => SimpleNameTest(name)
    }

  // See ws:explicit constraint.

  val wildcard: P[Wildcard] =
    P(Wildcards.wildcard)

  val kindTest: P[KindTest] =
    P(documentTest | elementTest | attributeTest | schemaElementTest | schemaAttributeTest | piTest | commentTest | textTest | namespaceNodeTest | anyKindTest)

  val documentTest: P[DocumentTest] =
    P(simpleDocumentTest | documentTestContainingElementTest | documentTestContainingSchemaElementTest)

  val simpleDocumentTest: P[SimpleDocumentTest.type] =
    P(NDT.documentNodeWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => SimpleDocumentTest)

  val documentTestContainingElementTest: P[DocumentTestContainingElementTest] =
    P(NDT.documentNodeWord ~ DT.openParenthesis ~ elementTest ~ DT.closeParenthesis) map {
      case elemTest => DocumentTestContainingElementTest(elemTest)
    }

  val documentTestContainingSchemaElementTest: P[DocumentTestContainingSchemaElementTest] =
    P(NDT.documentNodeWord ~ DT.openParenthesis ~ schemaElementTest ~ DT.closeParenthesis) map {
      case schemaElmTest => DocumentTestContainingSchemaElementTest(schemaElmTest)
    }

  val elementTest: P[ElementTest] =
    P(anyElementTest | elementNameTest | elementNameAndTypeTest | nillableElementNameAndTypeTest | elementTypeTest | nillableElementTypeTest)

  // Losing some efficiency on parsing of element tests

  val anyElementTest: P[AnyElementTest.type] =
    P(NDT.elementWord ~ DT.openParenthesis ~ DT.asterisk.? ~ DT.closeParenthesis) map (_ => AnyElementTest)

  val elementNameTest: P[ElementNameTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ eqName ~ DT.closeParenthesis) map {
      case name => ElementNameTest(name)
    }

  val elementNameAndTypeTest: P[ElementNameAndTypeTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ eqName ~ DT.comma ~ eqName ~ DT.closeParenthesis) map {
      case (name, tpe) => ElementNameAndTypeTest(name, tpe)
    }

  val nillableElementNameAndTypeTest: P[NillableElementNameAndTypeTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ eqName ~ DT.comma ~ eqName ~ DT.questionMark ~ DT.closeParenthesis) map {
      case (name, tpe) => NillableElementNameAndTypeTest(name, tpe)
    }

  val elementTypeTest: P[ElementTypeTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.comma ~ eqName ~ DT.closeParenthesis) map {
      case tpe => ElementTypeTest(tpe)
    }

  val nillableElementTypeTest: P[NillableElementTypeTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.comma ~ eqName ~ DT.questionMark ~ DT.closeParenthesis) map {
      case tpe => NillableElementTypeTest(tpe)
    }

  val attributeTest: P[AttributeTest] =
    P(anyAttributeTest | attributeNameTest | attributeNameAndTypeTest | attributeTypeTest)

  // Losing some efficiency on parsing of attribute tests

  val anyAttributeTest: P[AnyAttributeTest.type] =
    P(NDT.attributeWord ~ DT.openParenthesis ~ DT.asterisk.? ~ DT.closeParenthesis) map (_ => AnyAttributeTest)

  val attributeNameTest: P[AttributeNameTest] =
    P(NDT.attributeWord ~ DT.openParenthesis ~ eqName ~ DT.closeParenthesis) map {
      case name => AttributeNameTest(name)
    }

  val attributeNameAndTypeTest: P[AttributeNameAndTypeTest] =
    P(NDT.attributeWord ~ DT.openParenthesis ~ eqName ~ DT.comma ~ eqName ~ DT.closeParenthesis) map {
      case (name, tpe) => AttributeNameAndTypeTest(name, tpe)
    }

  val attributeTypeTest: P[AttributeTypeTest] =
    P(NDT.attributeWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.comma ~ eqName ~ DT.closeParenthesis) map {
      case tpe => AttributeTypeTest(tpe)
    }

  val schemaElementTest: P[SchemaElementTest] =
    P(NDT.schemaElementWord ~ DT.openParenthesis ~ eqName ~ DT.closeParenthesis) map {
      case name => SchemaElementTest(name)
    }

  val schemaAttributeTest: P[SchemaAttributeTest] =
    P(NDT.schemaAttributeWord ~ DT.openParenthesis ~ eqName ~ DT.closeParenthesis) map {
      case name => SchemaAttributeTest(name)
    }

  val piTest: P[PITest] =
    P(simplePiTest | targetPiTest | dataPiTest)

  val simplePiTest: P[SimplePITest.type] =
    P(NDT.processingInstructionWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => SimplePITest)

  val targetPiTest: P[TargetPITest] =
    P(NDT.processingInstructionWord ~ DT.openParenthesis ~ ncName ~ DT.closeParenthesis) map {
      case name => TargetPITest(name)
    }

  val dataPiTest: P[DataPITest] =
    P(NDT.processingInstructionWord ~ DT.openParenthesis ~ stringLiteral ~ DT.closeParenthesis) map {
      case stringLit => DataPITest(stringLit)
    }

  val commentTest: P[CommentTest.type] =
    P(NDT.commentWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => CommentTest)

  val textTest: P[TextTest.type] =
    P(NDT.textWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => TextTest)

  val namespaceNodeTest: P[NamespaceNodeTest.type] =
    P(NDT.namespaceNodeWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => NamespaceNodeTest)

  val anyKindTest: P[AnyKindTest.type] =
    P(NDT.nodeWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => AnyKindTest)

  // Postfix expressions

  val postfixExpr: P[PostfixExpr] =
    P(primaryExpr ~ (predicate | argumentList | lookup).rep) map {
      case (primaryExp, postfixes) => PostfixExpr(primaryExp, postfixes.toIndexedSeq)
    }

  val argumentList: P[ArgumentList] =
    P(DT.openParenthesis ~ argument.rep(sep = DT.comma) ~ DT.closeParenthesis) map {
      case args => ArgumentList(args.toIndexedSeq)
    }

  val argument: P[Argument] =
    P(argumentPlaceholder | exprSingleArgument)

  val argumentPlaceholder: P[ArgumentPlaceholder.type] =
    P(DT.questionMark) map (_ => ArgumentPlaceholder)

  val exprSingleArgument: P[ExprSingleArgument] =
    P(exprSingle) map {
      case exp => ExprSingleArgument(exp)
    }

  val lookup: P[PostfixLookup] =
    P(DT.questionMark ~ keySpecifier) map {
      case keySpec => PostfixLookup(keySpec)
    }

  val keySpecifier: P[KeySpecifier] =
    P(ncName | integerLiteral | DT.asterisk.! | parenthesizedExpr) map {
      case (nm: NCName) => NamedKeySpecifier(nm)
      case (intLit: IntegerLiteral) => PositionalKeySpecifier(intLit)
      case "*" => WildcardKeySpecifier
      case (exp: ParenthesizedExpr) => ParenthesizedExprKeySpecifier(exp)
    }

  val paramList: P[ParamList] =
    P(param.rep(min = 1, sep = DT.comma)) map {
      case pars => ParamList(pars.toIndexedSeq)
    }

  val param: P[Param] =
    P(DT.dollar ~ eqName ~ (NDT.asWord ~ sequenceType).?) map {
      case (name, tpeOption) => Param(name, tpeOption.map(t => TypeDeclaration(t)))
    }

  val predicate: P[Predicate] =
    P(DT.openBracket ~ expr ~ DT.closeBracket) map {
      case exp => Predicate(exp)
    }

  // Primary expressions

  // The branches of a primaryExpr are relatively easy to distinguish. See above.

  val primaryExpr: P[PrimaryExpr] =
    P(literal | varRef | parenthesizedExpr | contextItemExpr | functionCall | functionItemExpr |
      mapConstructor | arrayConstructor | unaryLookup)

  val literal: P[Literal] =
    P(stringLiteral | numericLiteral)

  // Using the StringLiterals.stringLiteral parser, etc.

  val stringLiteral: P[StringLiteral] =
    P(DT.stringLiteral)

  val numericLiteral: P[NumericLiteral] =
    P(NDT.numericLiteral)

  val integerLiteral: P[IntegerLiteral] =
    P(NDT.integerLiteral)

  val varRef: P[VarRef] =
    P(DT.dollar ~ eqName) map {
      name => VarRef(name)
    }

  val parenthesizedExpr: P[ParenthesizedExpr] =
    P(DT.openParenthesis ~ expr.? ~ DT.closeParenthesis) map {
      case expOption => ParenthesizedExpr(expOption)
    }

  val contextItemExpr: P[ContextItemExpr.type] =
    P(DT.dot) map (_ => ContextItemExpr)

  // See xgc:reserved-function-names
  // TODO gn:parens. This becomes important once we support comments.

  val functionCall: P[FunctionCall] =
    P(eqName.filter(nm => !ReservedFunctionNames.contains(nm)) ~ argumentList) map {
      case (name, argList) => FunctionCall(name, argList)
    }

  val functionItemExpr: P[FunctionItemExpr] =
    P(namedFunctionRef | inlineFunctionExpr)

  // See xgc:reserved-function-names

  val namedFunctionRef: P[NamedFunctionRef] =
    P(eqName.filter(nm => !ReservedFunctionNames.contains(nm)) ~ DT.hash ~ integerLiteral) map {
      case (name, arity) => NamedFunctionRef(name, arity.value)
    }

  val inlineFunctionExpr: P[InlineFunctionExpr] =
    P(NDT.functionWord ~ DT.openParenthesis ~ paramList.? ~ DT.closeParenthesis ~ (NDT.asWord ~ sequenceType).? ~ enclosedExpr) map {
      case (parListOption, resultTpeOption, body) =>
        InlineFunctionExpr(parListOption, resultTpeOption, body)
    }

  val mapConstructor: P[MapConstructor] =
    P(NDT.mapWord ~ DT.openBrace ~/ mapConstructorEntry.rep(sep = DT.comma) ~ DT.closeBrace) map {
      case entries => MapConstructor(entries.toIndexedSeq)
    }

  val mapConstructorEntry: P[MapConstructorEntry] =
    P(exprSingle ~ DT.colon ~ exprSingle) map {
      case (k, v) => MapConstructorEntry(k, v)
    }

  val arrayConstructor: P[ArrayConstructor] =
    P(squareArrayConstructor | curlyArrayConstructor)

  val squareArrayConstructor: P[SquareArrayConstructor] =
    P(DT.openBracket ~ exprSingle.rep(sep = DT.comma) ~ DT.closeBracket) map {
      case members => SquareArrayConstructor(members.toIndexedSeq)
    }

  val curlyArrayConstructor: P[CurlyArrayConstructor] =
    P(NDT.arrayWord ~ enclosedExpr) map {
      exp => CurlyArrayConstructor(exp)
    }

  val unaryLookup: P[UnaryLookup] =
    P(DT.questionMark ~ keySpecifier) map {
      case keySpec => UnaryLookup(keySpec)
    }

  // Types

  val sequenceType: P[SequenceType] =
    P(emptySequenceType | nonEmptySequenceType)

  val emptySequenceType: P[EmptySequenceType.type] =
    P(NDT.emptySequenceWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => EmptySequenceType)

  // TODO xgc:occurrence-indicators

  val nonEmptySequenceType: P[SequenceType] =
    P(itemType ~ (DT.questionMark | DT.asterisk | DT.plus).!.?) map {
      case (tpe, None) => ExactlyOneSequenceType(tpe)
      case (tpe, Some("?")) => ZeroOrOneSequenceType(tpe)
      case (tpe, Some("*")) => ZeroOrMoreSequenceType(tpe)
      case (tpe, Some("+")) => OneOrMoreSequenceType(tpe)
      case _ => EmptySequenceType
    }

  val itemType: P[ItemType] =
    P(kindTestItemType | anyItemType | anyFunctionTest | typedFunctionTest | atomicOrUnionType | parenthesizedItemType | mapTest | arrayTest)

  val kindTestItemType: P[KindTestItemType] =
    P(kindTest) map {
      case kindTst => KindTestItemType(kindTst)
    }

  val anyItemType: P[AnyItemType.type] =
    P(NDT.itemWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => AnyItemType)

  val anyFunctionTest: P[AnyFunctionTest.type] =
    P(NDT.functionWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.closeParenthesis) map (_ => AnyFunctionTest)

  val typedFunctionTest: P[TypedFunctionTest] =
    P(NDT.functionWord ~ DT.openParenthesis ~ sequenceType.rep(sep = DT.comma) ~ DT.closeParenthesis ~ NDT.asWord ~ sequenceType) map {
      case (parTpes, resultTpe) => TypedFunctionTest(parTpes.toIndexedSeq, resultTpe)
    }

  val atomicOrUnionType: P[AtomicOrUnionType] =
    P(eqName) map {
      case tpe => AtomicOrUnionType(tpe)
    }

  val parenthesizedItemType: P[ParenthesizedItemType] =
    P(DT.openParenthesis ~ itemType ~ DT.closeParenthesis) map {
      case tpe => ParenthesizedItemType(tpe)
    }

  val mapTest: P[MapTest] =
    P(anyMapTest | typedMapTest)

  val anyMapTest: P[AnyMapTest.type] =
    P(NDT.mapWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.closeParenthesis) map {
      case _ => AnyMapTest
    }

  val typedMapTest: P[TypedMapTest] =
    P(NDT.mapWord ~ DT.openParenthesis ~ atomicOrUnionType ~ DT.comma ~ sequenceType ~ DT.closeParenthesis) map {
      case (kt, vt) => TypedMapTest(kt, vt)
    }

  val arrayTest: P[ArrayTest] =
    P(anyArrayTest | typedArrayTest)

  val anyArrayTest: P[AnyArrayTest.type] =
    P(NDT.arrayWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.closeParenthesis) map {
      case _ => AnyArrayTest
    }

  val typedArrayTest: P[TypedArrayTest] =
    P(NDT.arrayWord ~ DT.openParenthesis ~ sequenceType ~ DT.closeParenthesis) map {
      case et => TypedArrayTest(et)
    }

  val singleType: P[SingleType] =
    P(eqName ~ DT.questionMark.!.?) map {
      case (tpe, None) => NonEmptySingleType(tpe)
      case (tpe, Some(_)) => PotentiallyEmptySingleType(tpe)
    }

  // Names (EQNames, NCNames etc.)
  // Using the NCNames.ncName and EQNames.eqName parsers

  private val ncName: P[NCName] = P(NCNames.ncName)

  private val eqName: P[EQName] = P(EQNames.eqName)

  // Operators etc.

  val valueComp: P[ValueComp] =
    P((NDT.eqWord | NDT.neWord | NDT.ltWord | NDT.leWord | NDT.gtWord | NDT.geWord).!) map (s => ValueComp.parse(s))

  val generalComp: P[GeneralComp] =
    P((DT.equals | DT.notEquals | DT.lessThan | DT.lessThanOrEqual |
      DT.greaterThan | DT.greaterThanOrEqual).!) map (s => GeneralComp.parse(s))

  val nodeComp: P[NodeComp] =
    P((NDT.isWord | DT.precedes | DT.follows).!) map (s => NodeComp.parse(s))

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
    EQName.QName("typeswitch"))
}
