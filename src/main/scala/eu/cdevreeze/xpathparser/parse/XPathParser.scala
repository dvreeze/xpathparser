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

import eu.cdevreeze.xpathparser.ast.BracedUriLiteral
import eu.cdevreeze.xpathparser.ast.EQName
import eu.cdevreeze.xpathparser.ast.NCName
import fastparse.WhitespaceApi

/**
 * XPath 3.0 parsing support, using FastParse.
 *
 * Usage: XPathParser.xpathExpr.parse(xpathString)
 *
 * TODO XPath 3.1.
 *
 * @author Chris de Vreeze
 */
object XPathParser {

  // TODO Improve, improve, improve. Study XPath spec more closely, use FastParse in a better way,
  // make code complete and more robust, improve the AST class hierarchy, etc.

  import eu.cdevreeze.xpathparser.ast._

  private val DT = DelimitingTerminals
  private val NDT = NonDelimitingTerminals

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._

    // TODO Adapt. What about parsing of comments?

    NoTrace(CharPred(c => java.lang.Character.isWhitespace(c)).rep)
  }

  import White._
  import fastparse.noApi._

  val xpathExpr: P[XPathExpr] =
    P(expr ~ End) map (e => XPathExpr(e))

  private val expr: P[Expr] =
    P(exprSingle.rep(min = 1, sep = DT.comma)) map {
      case (exprs) => Expr(exprs.toIndexedSeq)
    }

  private val enclosedExpr: P[EnclosedExpr] =
    P(DT.openBrace ~ expr ~ DT.closeBrace) map {
      case exp => EnclosedExpr(exp)
    }

  // The branches of exprSingle are easy to distinguish. All but one start with a different keyword.
  // Anything else must be an orExpr (if parsing succeeds).

  private val exprSingle: P[ExprSingle] =
    P(forExpr | letExpr | quantifiedExpr | ifExpr | orExpr)

  private val forExpr: P[ForExpr] =
    P(NDT.forWord ~/ simpleForBinding.rep(min = 1, sep = DT.comma) ~ NDT.returnWord ~ exprSingle) map {
      case (bindings, returnExp) => ForExpr(bindings.toIndexedSeq, returnExp)
    }

  private val simpleForBinding: P[SimpleForBinding] =
    P(DT.dollar ~ eqName ~ NDT.inWord ~ exprSingle) map {
      case (eqn, exp) => SimpleForBinding(eqn, exp)
    }

  private val letExpr: P[LetExpr] =
    P(NDT.letWord ~/ simpleLetBinding.rep(min = 1, sep = DT.comma) ~ NDT.returnWord ~ exprSingle) map {
      case (bindings, returnExp) => LetExpr(bindings.toIndexedSeq, returnExp)
    }

  private val simpleLetBinding: P[SimpleLetBinding] =
    P(DT.dollar ~ eqName ~ DT.assignmentSymbol ~ exprSingle) map {
      case (eqn, exp) => SimpleLetBinding(eqn, exp)
    }

  private val quantifiedExpr: P[QuantifiedExpr] =
    P((NDT.someWord | NDT.everyWord).! ~/ simpleBindingInQuantifiedExpr.rep(min = 1, sep = DT.comma) ~ NDT.satisfiesWord ~ exprSingle) map {
      case (quant, bindings, satisfiesExp) => QuantifiedExpr(Quantifier.parse(quant), bindings.toIndexedSeq, satisfiesExp)
    }

  private val simpleBindingInQuantifiedExpr: P[SimpleBindingInQuantifiedExpr] =
    P(DT.dollar ~ eqName ~ NDT.inWord ~ exprSingle) map {
      case (eqn, exp) => SimpleBindingInQuantifiedExpr(eqn, exp)
    }

  private val ifExpr: P[IfExpr] =
    P(NDT.ifWord ~/ DT.openParenthesis ~ expr ~ DT.closeParenthesis ~ NDT.thenWord ~ exprSingle ~ NDT.elseWord ~ exprSingle) map {
      case (e1, e2, e3) => IfExpr(e1, e2, e3)
    }

  private val orExpr: P[OrExpr] =
    P(andExpr.rep(min = 1, sep = NDT.orWord ~/ Pass)) map {
      case exps => OrExpr(exps.toIndexedSeq)
    }

  private val andExpr: P[AndExpr] =
    P(comparisonExpr.rep(min = 1, sep = NDT.andWord ~/ Pass)) map {
      case exps => AndExpr(exps.toIndexedSeq)
    }

  private val comparisonExpr: P[ComparisonExpr] =
    P(stringConcatExpr ~ ((valueComp | generalComp | nodeComp) ~/ stringConcatExpr).?) map {
      case (expr1, Some((op, expr2))) => CompoundComparisonExpr(expr1, op, expr2)
      case (expr, None)               => SimpleComparisonExpr(expr)
    }

  private val stringConcatExpr: P[StringConcatExpr] =
    P(rangeExpr.rep(min = 1, sep = DT.doubleVerticalBar ~/ Pass)) map {
      case exps => StringConcatExpr(exps.toIndexedSeq)
    }

  private val rangeExpr: P[RangeExpr] =
    P(additiveExpr ~ (NDT.toWord ~/ additiveExpr).?) map {
      case (additiveExp1, Some(additiveExp2)) => CompoundRangeExpr(additiveExp1, additiveExp2)
      case (additiveExp, None)                => SimpleRangeExpr(additiveExp)
    }

  private val additiveExpr: P[AdditiveExpr] =
    P(multiplicativeExpr ~ ((DT.plus | DT.minus).! ~/ multiplicativeExpr).rep) map {
      case (firstExp, opExpPairs) =>
        AdditiveExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => (AdditionOp.parse(kv._1) -> kv._2)))
    }

  private val multiplicativeExpr: P[MultiplicativeExpr] =
    P(unionExpr ~ ((DT.asterisk | (NDT.divWord | NDT.idivWord | NDT.modWord)).! ~/ unionExpr).rep) map {
      case (firstExp, opExpPairs) =>
        MultiplicativeExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => (MultiplicativeOp.parse(kv._1) -> kv._2)))
    }

  private val unionExpr: P[UnionExpr] =
    P(intersectExceptExpr ~ ((NDT.unionWord | DT.verticalBar) ~/ intersectExceptExpr).rep) map {
      case (expr, exprSeq) => UnionExpr(expr +: exprSeq.toIndexedSeq)
    }

  private val intersectExceptExpr: P[IntersectExceptExpr] =
    P(instanceOfExpr ~ ((NDT.intersectWord | NDT.exceptWord).! ~/ instanceOfExpr).rep) map {
      case (firstExp, opExpPairs) =>
        IntersectExceptExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => (IntersectExceptOp.parse(kv._1) -> kv._2)))
    }

  private val instanceOfExpr: P[InstanceOfExpr] =
    P(treatExpr ~ (NDT.instanceWord ~ NDT.ofWord ~/ sequenceType).?) map {
      case (expr, tpeOption) => InstanceOfExpr(expr, tpeOption)
    }

  private val treatExpr: P[TreatExpr] =
    P(castableExpr ~ (NDT.treatWord ~ NDT.asWord ~/ sequenceType).?) map {
      case (expr, tpeOption) => TreatExpr(expr, tpeOption)
    }

  private val castableExpr: P[CastableExpr] =
    P(castExpr ~ (NDT.castableWord ~ NDT.asWord ~/ singleType).?) map {
      case (expr, tpeOption) => CastableExpr(expr, tpeOption)
    }

  private val castExpr: P[CastExpr] =
    P(unaryExpr ~ (NDT.castWord ~ NDT.asWord ~/ singleType).?) map {
      case (expr, tpeOption) => CastExpr(expr, tpeOption)
    }

  private val unaryExpr: P[UnaryExpr] =
    P((DT.minus | DT.plus).!.rep ~ valueExpr) map {
      case (ops, expr) => UnaryExpr(ops.toIndexedSeq.map(op => UnaryOp.parse(op)), expr)
    }

  private val valueExpr: P[ValueExpr] =
    P(simpleMapExpr) map {
      case expr => ValueExpr(expr)
    }

  private val simpleMapExpr: P[SimpleMapExpr] =
    P(pathExpr.rep(min = 1, sep = DT.exclamationMark)) map {
      case exps => SimpleMapExpr(exps.toIndexedSeq)
    }

  // According to constraint xgc:leading-lone-slash, we need to look ahead just one token to determine if a slash is a path
  // expression or if it has to be taken together with the relative path expression that must come after the slash.
  // Note that a relativePathExpr can never start with a slash (partly because an EQName cannot start with a slash).
  // Hence the 4 branches below are easy to distinguish.

  private val pathExpr: P[PathExpr] =
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
  // NCName or URI-qualified name or the token "function". (Note that, like context items, decimal and double literals may start with dots.)

  private val canStartPostfixExpr: P[Unit] =
    P(literal | varRef | DT.openParenthesis | contextItemExpr | eqName | NDT.functionWord).map(_ => ())

  // Looking ahead to distinguish a single slash from a double slash, and to recognize the start of a relativePathExpr.
  // See xgc:leading-lone-slash constraint.

  private val slashOnlyPathExpr: P[PathExpr] =
    P(DT.slash ~ !canStartRelativePathExpr) map {
      case _ => SlashOnlyPathExpr
    }

  // See above. Note that the next token is not a slash, because 2 slashes together make up one token,
  // and because canStartRelativePathExpr implies that the next token cannot be a slash anyway.

  private val pathExprStartingWithSingleSlash: P[PathExpr] =
    P(DT.slash ~ &(canStartRelativePathExpr) ~ relativePathExpr) map {
      case expr => PathExprStartingWithSingleSlash(expr)
    }

  private val pathExprStartingWithDoubleSlash: P[PathExpr] =
    P(DT.doubleSlash ~ relativePathExpr) map {
      case expr => PathExprStartingWithDoubleSlash(expr)
    }

  private val relativePathExpr: P[RelativePathExpr] =
    P(stepExpr ~ ((DT.slash | DT.doubleSlash).! ~/ stepExpr).rep) map {
      case (firstExp, opExpPairs) =>
        RelativePathExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => (StepOp.parse(kv._1) -> kv._2)))
    }

  // The 2 branches of a stepExpr are relatively easy to distinguish. Note that both branches may start with an EQName (or "keyword"), and other than that
  // only start with mutually exclusive tokens. The difference between the 2 branches is that an axisStep starting with an EQName only contains the EQName,
  // whereas a postfixExpr may start but may never end with an EQName. Each postfixExpr starting with an EQName is a function call or named function
  // reference. Two constraints (xgc:reserved-function-names and gn:parens) further help in recognizing function calls and named function references.

  // Hence, we first try the branch for postfixExpr, and try branch axisStep if the first one fails.

  private val stepExpr: P[StepExpr] =
    P(postfixExpr | axisStep)

  // The 2 branches of an axisStep are relatively easy to distinguish. A reverseAxisStep is easy to recognize.
  // A forwardAxisStep is easy to recognize if non-abbreviated, and otherwise it starts with a nodeTest, possibly
  // preceded by "@".

  // We first try the reverseAxisStep, and only then the forwardAxisStep, to make sure that nodeTests are only
  // tried if all other options (like non-abbreviated steps) do not apply. Note that the lookahead needed for
  // discarding reverseAxisStep is limited (2 tokens).

  private val axisStep: P[AxisStep] =
    P(reverseAxisStep | forwardAxisStep)

  private val forwardAxisStep: P[ForwardAxisStep] =
    P(forwardStep ~ predicate.rep) map {
      case (forwardStep, predicates) => ForwardAxisStep(forwardStep, predicates.toIndexedSeq)
    }

  private val reverseAxisStep: P[ReverseAxisStep] =
    P(reverseStep ~ predicate.rep) map {
      case (reverseStep, predicates) => ReverseAxisStep(reverseStep, predicates.toIndexedSeq)
    }

  private val forwardStep: P[ForwardStep] =
    P(nonAbbrevForwardStep | abbrevForwardStep)

  private val abbrevForwardStep: P[AbbrevForwardStep] =
    P(simpleAbbrevForwardStep | attributeAxisAbbrevForwardStep)

  private val simpleAbbrevForwardStep: P[SimpleAbbrevForwardStep] =
    P(nodeTest) map {
      case nodeTest => SimpleAbbrevForwardStep(nodeTest)
    }

  private val attributeAxisAbbrevForwardStep: P[AttributeAxisAbbrevForwardStep] =
    P(DT.at ~ nodeTest) map {
      case nodeTest => AttributeAxisAbbrevForwardStep(nodeTest)
    }

  private val nonAbbrevForwardStep: P[NonAbbrevForwardStep] =
    P(forwardAxis ~/ nodeTest) map {
      case (axis, nodeTest) => NonAbbrevForwardStep(axis, nodeTest)
    }

  private val forwardAxis: P[ForwardAxis] =
    P((NDT.childWord | NDT.descendantWord | NDT.attributeWord | NDT.selfWord | NDT.descendantOrSelfWord |
      NDT.followingSiblingWord | NDT.followingWord | NDT.namespaceWord).! ~ DT.doubleColon) map {

      case "child"              => ForwardAxis.Child
      case "descendant"         => ForwardAxis.Descendant
      case "attribute"          => ForwardAxis.Attribute
      case "self"               => ForwardAxis.Self
      case "descendant-or-self" => ForwardAxis.DescendantOrSelf
      case "following-sibling"  => ForwardAxis.FollowingSibling
      case "following"          => ForwardAxis.Following
      case "namespace"          => ForwardAxis.Namespace
    }

  private val reverseStep: P[ReverseStep] =
    P(nonAbbrevReverseStep | abbrevReverseStep)

  private val abbrevReverseStep: P[AbbrevReverseStep.type] =
    P(DT.doubleDot) map (_ => AbbrevReverseStep)

  private val nonAbbrevReverseStep: P[NonAbbrevReverseStep] =
    P(reverseAxis ~/ nodeTest) map {
      case (axis, nodeTest) => NonAbbrevReverseStep(axis, nodeTest)
    }

  private val reverseAxis: P[ReverseAxis] =
    P((NDT.parentWord | NDT.ancestorWord | NDT.precedingSiblingWord | NDT.precedingWord | NDT.ancestorOrSelfWord).! ~ DT.doubleColon) map {
      case "parent"            => ReverseAxis.Parent
      case "ancestor"          => ReverseAxis.Ancestor
      case "preceding-sibling" => ReverseAxis.PrecedingSibling
      case "preceding"         => ReverseAxis.Preceding
      case "ancestor-or-self"  => ReverseAxis.AncestorOrSelf
    }

  // The 2 branches of a nodeTest are easy to distinguish, with limited lookahead.
  // We first try branch kindTest, which always starts with a "keyword". If that fails, we try the nameTest branch.

  private val nodeTest: P[NodeTest] =
    P(kindTest | nameTest)

  // The 2 branches of a nameTest are relatively easy to distinguish. A simpleNameTest is just an EQName, whereas a wildcard
  // always contains an asterisk. Also mind the remarks below.

  // As per the grammar specification, we first try the simpleNameTest branch, and then, if it fails, the wildcard branch.
  // This way a URI qualified name will be recognized before the "braced URI literal wildcard". Because of the ws:explicit
  // constraint a prefix wildcard will not be "hidden" by a QName as EQName.

  private val nameTest: P[NameTest] =
    P(simpleNameTest | wildcard)

  private val simpleNameTest: P[SimpleNameTest] =
    P(eqName) map {
      case name => SimpleNameTest(name)
    }

  // See ws:explicit constraint.

  private val wildcard: P[Wildcard] =
    P(anyWildcard | prefixWildcard | localNameWildcard | namespaceWildcard)

  private val anyWildcard: P[AnyWildcard.type] =
    P(CharsWhileIn("*:").!) filter (s => s == "*") map (_ => AnyWildcard)

  private val prefixWildcard: P[PrefixWildcard] =
    P(CharsWhile(isNCNameCharOrColonOrStar).!) filter (isPrefixWildcard) map (v => PrefixWildcard(NCName(v.dropRight(2))))

  private val localNameWildcard: P[LocalNameWildcard] =
    P(CharsWhile(isNCNameCharOrColonOrStar).!) filter (isLocalNameWildcard) map (v => LocalNameWildcard(NCName(v.drop(2))))

  private val namespaceWildcard: P[NamespaceWildcard] =
    P(CharsWhile(isNCNameCharOrBraceOrStar).!) filter (isNamespaceWildcard) map (v => NamespaceWildcard(BracedUriLiteral.parse(v.dropRight(1))))

  private val kindTest: P[KindTest] =
    P(documentTest | elementTest | attributeTest | schemaElementTest | schemaAttributeTest | piTest | commentTest | textTest | namespaceNodeTest | anyKindTest)

  private val documentTest: P[DocumentTest] =
    P(simpleDocumentTest | documentTestContainingElementTest | documentTestContainingSchemaElementTest)

  private val simpleDocumentTest: P[SimpleDocumentTest.type] =
    P(NDT.documentNodeWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => SimpleDocumentTest)

  private val documentTestContainingElementTest: P[DocumentTestContainingElementTest] =
    P(NDT.documentNodeWord ~ DT.openParenthesis ~ elementTest ~ DT.closeParenthesis) map {
      case elemTest => DocumentTestContainingElementTest(elemTest)
    }

  private val documentTestContainingSchemaElementTest: P[DocumentTestContainingSchemaElementTest] =
    P(NDT.documentNodeWord ~ DT.openParenthesis ~ schemaElementTest ~ DT.closeParenthesis) map {
      case schemaElmTest => DocumentTestContainingSchemaElementTest(schemaElmTest)
    }

  private val elementTest: P[ElementTest] =
    P(anyElementTest | elementNameTest | elementNameAndTypeTest | nillableElementNameAndTypeTest | elementTypeTest | nillableElementTypeTest)

  // Losing some efficiency on parsing of element tests

  private val anyElementTest: P[AnyElementTest.type] =
    P(NDT.elementWord ~ DT.openParenthesis ~ DT.asterisk.? ~ DT.closeParenthesis) map (_ => AnyElementTest)

  private val elementNameTest: P[ElementNameTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ eqName ~ DT.closeParenthesis) map {
      case name => ElementNameTest(name)
    }

  private val elementNameAndTypeTest: P[ElementNameAndTypeTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ eqName ~ DT.comma ~ eqName ~ DT.closeParenthesis) map {
      case (name, tpe) => ElementNameAndTypeTest(name, tpe)
    }

  private val nillableElementNameAndTypeTest: P[NillableElementNameAndTypeTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ eqName ~ DT.comma ~ eqName ~ DT.questionMark ~ DT.closeParenthesis) map {
      case (name, tpe) => NillableElementNameAndTypeTest(name, tpe)
    }

  private val elementTypeTest: P[ElementTypeTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.comma ~ eqName ~ DT.closeParenthesis) map {
      case tpe => ElementTypeTest(tpe)
    }

  private val nillableElementTypeTest: P[NillableElementTypeTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.comma ~ eqName ~ DT.questionMark ~ DT.closeParenthesis) map {
      case tpe => NillableElementTypeTest(tpe)
    }

  private val attributeTest: P[AttributeTest] =
    P(anyAttributeTest | attributeNameTest | attributeNameAndTypeTest | attributeTypeTest)

  // Losing some efficiency on parsing of attribute tests

  private val anyAttributeTest: P[AnyAttributeTest.type] =
    P(NDT.attributeWord ~ DT.openParenthesis ~ DT.asterisk.? ~ DT.closeParenthesis) map (_ => AnyAttributeTest)

  private val attributeNameTest: P[AttributeNameTest] =
    P(NDT.attributeWord ~ DT.openParenthesis ~ eqName ~ DT.closeParenthesis) map {
      case name => AttributeNameTest(name)
    }

  private val attributeNameAndTypeTest: P[AttributeNameAndTypeTest] =
    P(NDT.attributeWord ~ DT.openParenthesis ~ eqName ~ DT.comma ~ eqName ~ DT.closeParenthesis) map {
      case (name, tpe) => AttributeNameAndTypeTest(name, tpe)
    }

  private val attributeTypeTest: P[AttributeTypeTest] =
    P(NDT.attributeWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.comma ~ eqName ~ DT.closeParenthesis) map {
      case tpe => AttributeTypeTest(tpe)
    }

  private val schemaElementTest: P[SchemaElementTest] =
    P(NDT.schemaElementWord ~ DT.openParenthesis ~ eqName ~ DT.closeParenthesis) map {
      case name => SchemaElementTest(name)
    }

  private val schemaAttributeTest: P[SchemaAttributeTest] =
    P(NDT.schemaAttributeWord ~ DT.openParenthesis ~ eqName ~ DT.closeParenthesis) map {
      case name => SchemaAttributeTest(name)
    }

  private val piTest: P[PITest] =
    P(simplePiTest | targetPiTest | dataPiTest)

  private val simplePiTest: P[SimplePITest.type] =
    P(NDT.processingInstructionWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => SimplePITest)

  private val targetPiTest: P[TargetPITest] =
    P(NDT.processingInstructionWord ~ DT.openParenthesis ~ ncName ~ DT.closeParenthesis) map {
      case name => TargetPITest(name)
    }

  private val dataPiTest: P[DataPITest] =
    P(NDT.processingInstructionWord ~ DT.openParenthesis ~ stringLiteral ~ DT.closeParenthesis) map {
      case stringLit => DataPITest(stringLit)
    }

  private val commentTest: P[CommentTest.type] =
    P(NDT.commentWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => CommentTest)

  private val textTest: P[TextTest.type] =
    P(NDT.textWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => TextTest)

  private val namespaceNodeTest: P[NamespaceNodeTest.type] =
    P(NDT.namespaceNodeWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => NamespaceNodeTest)

  private val anyKindTest: P[AnyKindTest.type] =
    P(NDT.nodeWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => AnyKindTest)

  private val postfixExpr: P[PostfixExpr] =
    P(primaryExpr ~ (predicate | argumentList).rep) map {
      case (primaryExp, predicateOrArgumentListSeq) => PostfixExpr(primaryExp, predicateOrArgumentListSeq.toIndexedSeq)
    }

  private val argumentList: P[ArgumentList] =
    P(DT.openParenthesis ~ argument.rep(sep = DT.comma) ~ DT.closeParenthesis) map {
      case args => ArgumentList(args.toIndexedSeq)
    }

  private val argument: P[Argument] =
    P(argumentPlaceholder | exprSingleArgument)

  private val argumentPlaceholder: P[ArgumentPlaceholder.type] =
    P(DT.questionMark) map (_ => ArgumentPlaceholder)

  private val exprSingleArgument: P[ExprSingleArgument] =
    P(exprSingle) map {
      case exp => ExprSingleArgument(exp)
    }

  private val paramList: P[ParamList] =
    P(param.rep(min = 1, sep = DT.comma)) map {
      case pars => ParamList(pars.toIndexedSeq)
    }

  private val param: P[Param] =
    P(DT.dollar ~ eqName ~ (NDT.asWord ~ sequenceType).?) map {
      case (name, tpeOption) => Param(name, tpeOption.map(t => TypeDeclaration(t)))
    }

  private val predicate: P[Predicate] =
    P(DT.openBracket ~ expr ~ DT.closeBracket) map {
      case exp => Predicate(exp)
    }

  // Primary expressions

  // The branches of a primaryExpr are relatively easy to distinguish. See above.

  private val primaryExpr: P[PrimaryExpr] =
    P(literal | varRef | parenthesizedExpr | contextItemExpr | functionCall | functionItemExpr)

  private val literal: P[Literal] =
    P(stringLiteral | numericLiteral)

  // Using the StringLiterals.stringLiteral parser, etc.

  private val stringLiteral: P[StringLiteral] =
    P(DT.stringLiteral)

  private val numericLiteral: P[NumericLiteral] =
    P(NDT.numericLiteral)

  private val integerLiteral: P[IntegerLiteral] =
    P(NDT.integerLiteral)

  private val varRef: P[VarRef] =
    P(DT.dollar ~ eqName) map {
      name => VarRef(name)
    }

  private val parenthesizedExpr: P[ParenthesizedExpr] =
    P(DT.openParenthesis ~ expr.? ~ DT.closeParenthesis) map {
      case expOption => ParenthesizedExpr(expOption)
    }

  private val contextItemExpr: P[ContextItemExpr.type] =
    P(DT.dot) map (_ => ContextItemExpr)

  // See xgc:reserved-function-names
  // TODO gn:parens. This becomes important once we support comments.

  private val functionCall: P[FunctionCall] =
    P(eqName.filter(nm => !ReservedFunctionNames.contains(nm)) ~ argumentList) map {
      case (name, argList) => FunctionCall(name, argList)
    }

  private val functionItemExpr: P[FunctionItemExpr] =
    P(namedFunctionRef | inlineFunctionExpr)

  // See xgc:reserved-function-names

  private val namedFunctionRef: P[NamedFunctionRef] =
    P(eqName.filter(nm => !ReservedFunctionNames.contains(nm)) ~ DT.hash ~ integerLiteral) map {
      case (name, arity) => NamedFunctionRef(name, arity.value)
    }

  private val inlineFunctionExpr: P[InlineFunctionExpr] =
    P(NDT.functionWord ~ DT.openParenthesis ~ paramList.? ~ DT.closeParenthesis ~ (NDT.asWord ~ sequenceType).? ~ enclosedExpr) map {
      case (parListOption, resultTpeOption, body) =>
        InlineFunctionExpr(parListOption, resultTpeOption, body)
    }

  // Types

  private val sequenceType: P[SequenceType] =
    P(emptySequenceType | nonEmptySequenceType)

  private val emptySequenceType: P[EmptySequenceType.type] =
    P(NDT.emptySequenceWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => EmptySequenceType)

  // TODO xgc:occurrence-indicators

  private val nonEmptySequenceType: P[SequenceType] =
    P(itemType ~ (DT.questionMark | DT.asterisk | DT.plus).!.?) map {
      case (tpe, None)      => ExactlyOneSequenceType(tpe)
      case (tpe, Some("?")) => ZeroOrOneSequenceType(tpe)
      case (tpe, Some("*")) => ZeroOrMoreSequenceType(tpe)
      case (tpe, Some("+")) => OneOrMoreSequenceType(tpe)
      case _                => EmptySequenceType
    }

  private val itemType: P[ItemType] =
    P(kindTestItemType | anyItemType | anyFunctionTest | typedFunctionTest | atomicOrUnionType | parenthesizedItemType)

  private val kindTestItemType: P[KindTestItemType] =
    P(kindTest) map {
      case kindTst => KindTestItemType(kindTst)
    }

  private val anyItemType: P[AnyItemType.type] =
    P(NDT.itemWord ~ DT.openParenthesis ~ DT.closeParenthesis) map (_ => AnyItemType)

  private val anyFunctionTest: P[AnyFunctionTest.type] =
    P(NDT.functionWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.closeParenthesis) map (_ => AnyFunctionTest)

  private val typedFunctionTest: P[TypedFunctionTest] =
    P(NDT.functionWord ~ DT.openParenthesis ~ sequenceType.rep(sep = DT.comma) ~ DT.closeParenthesis ~ NDT.asWord ~ sequenceType) map {
      case (parTpes, resultTpe) => TypedFunctionTest(parTpes.toIndexedSeq, resultTpe)
    }

  private val atomicOrUnionType: P[AtomicOrUnionType] =
    P(eqName) map {
      case tpe => AtomicOrUnionType(tpe)
    }

  private val parenthesizedItemType: P[ParenthesizedItemType] =
    P(DT.openParenthesis ~ itemType ~ DT.closeParenthesis) map {
      case tpe => ParenthesizedItemType(tpe)
    }

  private val singleType: P[SingleType] =
    P(eqName ~ DT.questionMark.!.?) map {
      case (tpe, None)    => NonEmptySingleType(tpe)
      case (tpe, Some(_)) => PotentiallyEmptySingleType(tpe)
    }

  // Names (EQNames, NCNames etc.)
  // Using the NCNames.ncName and EQNames.eqName parsers

  private val ncName: P[NCName] = P(NCNames.ncName)

  private val eqName: P[EQName] = P(EQNames.eqName)

  // Operators etc.

  private val valueComp: P[ValueComp] =
    P((NDT.eqWord | NDT.neWord | NDT.ltWord | NDT.leWord | NDT.gtWord | NDT.geWord).!) map (s => ValueComp.parse(s))

  private val generalComp: P[GeneralComp] =
    P((DT.equals | DT.notEquals | DT.lessThan | DT.lessThanOrEqual |
      DT.greaterThan | DT.greaterThanOrEqual).!) map (s => GeneralComp.parse(s))

  private val nodeComp: P[NodeComp] =
    P((NDT.isWord | DT.precedes | DT.follows).!) map (s => NodeComp.parse(s))

  // Utility methods (and data)

  private val ReservedFunctionNames: Set[EQName] = Set(
    EQName.QName("attribute"),
    EQName.QName("comment"),
    EQName.QName("document-node"),
    EQName.QName("element"),
    EQName.QName("empty-sequence"),
    EQName.QName("function"),
    EQName.QName("if"),
    EQName.QName("item"),
    EQName.QName("namespace-node"),
    EQName.QName("node"),
    EQName.QName("processing-instruction"),
    EQName.QName("schema-attribute"),
    EQName.QName("schema-element"),
    EQName.QName("switch"),
    EQName.QName("text"),
    EQName.QName("typeswitch"))

  private def isPrefixWildcard(s: String): Boolean = {
    s.endsWith(":*") && NCName.canBeNCName(s.dropRight(2))
  }

  private def isLocalNameWildcard(s: String): Boolean = {
    s.startsWith("*:") && NCName.canBeNCName(s.drop(2))
  }

  private def isNamespaceWildcard(s: String): Boolean = {
    s.endsWith("*") && BracedUriLiteral.canBeBracedUriLiteral(s.dropRight(1))
  }

  private def isNCNameCharOrColonOrStar(c: Char): Boolean = {
    NCName.canBePartOfNCName(c) || (c == ':') || (c == '*')
  }

  private def isNCNameCharOrBraceOrStar(c: Char): Boolean = {
    NCName.canBePartOfNCName(c) || (c == '{') || (c == '}') || (c == '*')
  }
}
