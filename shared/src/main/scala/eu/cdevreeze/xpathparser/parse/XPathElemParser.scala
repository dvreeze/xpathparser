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
import fastparse.MultiLineWhitespace._

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
 * Using the parsers in XPathElemParser may be somewhat risky in that they may "malfunction" when called in isolation,
 * due to the lack of context (such as cuts to avoid backtracking). Usually it is safer to stick to using the
 * XPathParser.xpathExpr parser. On the other hand, exposing parsers for specific AST elements makes it easier to
 * "decorate" specific parsers.
 *
 * TODO Make this the default implementation of a parser interface.
 *
 * @author Chris de Vreeze
 */
object XPathElemParser {

  // TODO Improve, improve, improve. Study XPath spec more closely, use FastParse in a better way.
  // TODO Also make code complete and more robust, improve the AST class hierarchy, etc.

  private val DT = DelimitingTerminals
  private val NDT = NonDelimitingTerminals

  // TODO Rethink whitespace (multi/single-line, comments etc.)

  import fastparse._

  def expr[_: P]: P[Expr] =
    P(exprSingle.rep(min = 1, sep = DT.comma)).map {
      exprs => Expr(NonEmptyVector.fromVectorUnsafe(exprs.toVector))
    }

  def enclosedExpr[_: P]: P[EnclosedExpr] =
    P(DT.openBrace ~ expr.? ~ DT.closeBrace).map {
      expOpt => EnclosedExpr(expOpt)
    }

  // The branches of exprSingle are easy to distinguish. All but one start with a different keyword.
  // Anything else must be an orExpr (if parsing succeeds).

  def exprSingle[_: P]: P[ExprSingle] =
    P(forExpr | letExpr | quantifiedExpr | ifExpr | orExpr)

  def forExpr[_: P]: P[ForExpr] =
    P(NDT.forWord ~/ simpleForBinding.rep(min = 1, sep = DT.comma) ~ NDT.returnWord ~ exprSingle).map {
      case (bindings, returnExp) => ForExpr(NonEmptyVector.fromVectorUnsafe(bindings.toVector), returnExp)
    }

  def simpleForBinding[_: P]: P[SimpleForBinding] =
    P(DT.dollar ~ eqName ~ NDT.inWord ~ exprSingle).map {
      case (eqn, exp) => SimpleForBinding(eqn, exp)
    }

  def letExpr[_: P]: P[LetExpr] =
    P(NDT.letWord ~/ simpleLetBinding.rep(min = 1, sep = DT.comma) ~ NDT.returnWord ~ exprSingle).map {
      case (bindings, returnExp) => LetExpr(NonEmptyVector.fromVectorUnsafe(bindings.toVector), returnExp)
    }

  def simpleLetBinding[_: P]: P[SimpleLetBinding] =
    P(DT.dollar ~ eqName ~ DT.assignmentSymbol ~ exprSingle).map {
      case (eqn, exp) => SimpleLetBinding(eqn, exp)
    }

  def quantifiedExpr[_: P]: P[QuantifiedExpr] =
    P((NDT.someWord | NDT.everyWord).! ~/ simpleBindingInQuantifiedExpr.rep(min = 1, sep = DT.comma) ~ NDT.satisfiesWord ~ exprSingle).map {
      case (quant, bindings, satisfiesExp) => QuantifiedExpr(Quantifier.parse(quant), NonEmptyVector.fromVectorUnsafe(bindings.toVector), satisfiesExp)
    }

  def simpleBindingInQuantifiedExpr[_: P]: P[SimpleBindingInQuantifiedExpr] =
    P(DT.dollar ~ eqName ~ NDT.inWord ~ exprSingle).map {
      case (eqn, exp) => SimpleBindingInQuantifiedExpr(eqn, exp)
    }

  def ifExpr[_: P]: P[IfExpr] =
    P(NDT.ifWord ~/ DT.openParenthesis ~ expr ~ DT.closeParenthesis ~ NDT.thenWord ~ exprSingle ~ NDT.elseWord ~ exprSingle).map {
      case (e1, e2, e3) => IfExpr(e1, e2, e3)
    }

  def orExpr[_: P]: P[OrExpr] =
    P(andExpr.rep(min = 1, sep = NDT.orWord ~/ Pass)).map {
      exps => OrExpr(NonEmptyVector.fromVectorUnsafe(exps.toVector))
    }

  def andExpr[_: P]: P[AndExpr] =
    P(comparisonExpr.rep(min = 1, sep = NDT.andWord ~/ Pass)).map {
      exps => AndExpr(NonEmptyVector.fromVectorUnsafe(exps.toVector))
    }

  def comparisonExpr[_: P]: P[ComparisonExpr] =
    P(stringConcatExpr ~ (comp ~/ stringConcatExpr).?).map {
      case (expr1, Some((op, expr2))) => CompoundComparisonExpr(expr1, op, expr2)
      case (expr, None) => expr
    }

  def stringConcatExpr[_: P]: P[StringConcatExpr] =
    P(rangeExpr.rep(min = 1, sep = DT.doubleVerticalBar ~/ Pass)).map {
      exps => StringConcatExpr(NonEmptyVector.fromVectorUnsafe(exps.toVector))
    }

  def rangeExpr[_: P]: P[RangeExpr] =
    P(additiveExpr ~ (NDT.toWord ~/ additiveExpr).?).map {
      case (additiveExp1, Some(additiveExp2)) => CompoundRangeExpr(additiveExp1, additiveExp2)
      case (additiveExp, None) => additiveExp
    }

  def additiveExpr[_: P]: P[AdditiveExpr] =
    P(multiplicativeExpr ~ ((DT.plus | DT.minus).! ~/ multiplicativeExpr).rep).map {
      case (firstExp, opExpPairs) =>
        AdditiveExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => AdditionOp.parse(kv._1) -> kv._2))
    }

  def multiplicativeExpr[_: P]: P[MultiplicativeExpr] =
    P(unionExpr ~ ((DT.asterisk | (NDT.divWord | NDT.idivWord | NDT.modWord)).! ~/ unionExpr).rep).map {
      case (firstExp, opExpPairs) =>
        MultiplicativeExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => MultiplicativeOp.parse(kv._1) -> kv._2))
    }

  def unionExpr[_: P]: P[UnionExpr] =
    P(intersectExceptExpr ~ ((NDT.unionWord | DT.verticalBar) ~/ intersectExceptExpr).rep).map {
      case (expr, exprSeq) => UnionExpr(NonEmptyVector.fromVectorUnsafe(exprSeq.toVector.prepended(expr)))
    }

  def intersectExceptExpr[_: P]: P[IntersectExceptExpr] =
    P(instanceOfExpr ~ ((NDT.intersectWord | NDT.exceptWord).! ~/ instanceOfExpr).rep).map {
      case (firstExp, opExpPairs) =>
        IntersectExceptExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => IntersectExceptOp.parse(kv._1) -> kv._2))
    }

  def instanceOfExpr[_: P]: P[InstanceOfExpr] =
    P(treatExpr ~ (NDT.instanceWord ~ NDT.ofWord ~/ sequenceType).?).map {
      case (expr, tpeOption) => InstanceOfExpr(expr, tpeOption)
    }

  def treatExpr[_: P]: P[TreatExpr] =
    P(castableExpr ~ (NDT.treatWord ~ NDT.asWord ~/ sequenceType).?).map {
      case (expr, tpeOption) => TreatExpr(expr, tpeOption)
    }

  def castableExpr[_: P]: P[CastableExpr] =
    P(castExpr ~ (NDT.castableWord ~ NDT.asWord ~/ singleType).?).map {
      case (expr, tpeOption) => CastableExpr(expr, tpeOption)
    }

  def castExpr[_: P]: P[CastExpr] =
    P(arrowExpr ~ (NDT.castWord ~ NDT.asWord ~/ singleType).?).map {
      case (expr, tpeOption) => CastExpr(expr, tpeOption)
    }

  def arrowExpr[_: P]: P[ArrowExpr] =
    P(unaryExpr ~ arrowFunctionCall.rep).map {
      case (exp, funCalls) => ArrowExpr(exp, funCalls.toIndexedSeq)
    }

  def arrowFunctionCall[_: P]: P[ArrowFunctionCall] =
    P(DT.doubleArrow ~/ arrowFunctionSpecifier ~ argumentList).map {
      case (funcSpec, args) => ArrowFunctionCall(funcSpec, args)
    }

  def arrowFunctionSpecifier[_: P]: P[ArrowFunctionSpecifier] =
    P(eqName | varRef | parenthesizedExpr).map {
      case nm: EQName => EQNameAsArrowFunctionSpecifier(nm)
      case ref@VarRef(_) => VarRefAsArrowFunctionSpecifier(ref)
      case exp@ParenthesizedExpr(_) => ParenthesizedExprAsArrowFunctionSpecifier(exp)
    }

  def unaryExpr[_: P]: P[UnaryExpr] =
    P((DT.minus | DT.plus).!.rep ~ valueExpr).map {
      case (ops, expr) => UnaryExpr(ops.toIndexedSeq.map(op => UnaryOp.parse(op)), expr)
    }

  def valueExpr[_: P]: P[ValueExpr] =
    P(simpleMapExpr)

  def simpleMapExpr[_: P]: P[SimpleMapExpr] =
    P(pathExpr.rep(min = 1, sep = DT.exclamationMark)).map {
      exps => SimpleMapExpr(NonEmptyVector.fromVectorUnsafe(exps.toVector))
    }

  // According to constraint xgc:leading-lone-slash, we need to look ahead just one token to determine if a slash is a path
  // expression or if it has to be taken together with the relative path expression that must come after the slash.
  // Note that a relativePathExpr can never start with a slash (partly because an EQName cannot start with a slash).
  // Hence the 4 branches below are easy to distinguish.

  def pathExpr[_: P]: P[PathExpr] =
    P(slashOnlyPathExpr | pathExprStartingWithSingleSlash | pathExprStartingWithDoubleSlash | relativePathExpr)

  // Lookahead parsers, to determine if the next token can start a relative path expression.
  // For these lookahead parsers, it is not important to distinguish branch canStartAxisStep from canStartPostfixExpr.

  private def canStartRelativePathExpr[_: P]: P[Unit] =
    P(canStartAxisStep | canStartPostfixExpr)

  // The start of an axis step is easy to recognize, unless it is a nodeTest. The latter is a kindTest (easy to recognize),
  // wildcard or EQName. The EQName variant makes it harder to distinguish an axisStep from a postfixExpr.

  private def canStartAxisStep[_: P]: P[Unit] =
    P(forwardAxis | reverseAxis | DT.at | DT.doubleDot | nodeTest).map(_ => ())

  // A postfix expression starts with a (string or numeric) literal, dollar sign, (opening) parenthesis, dot,
  // NCName or URI-qualified name or the token "function". In XPath 3.1 it can also start with token "map" or "array",
  // an open bracket, or a question mark. (Note that, like context items, decimal and double literals may start with dots.)

  private def canStartPostfixExpr[_: P]: P[Unit] =
    P(literal | varRef | DT.openParenthesis | contextItemExpr | eqName |
      NDT.functionWord | NDT.mapWord | NDT.arrayWord | DT.openBracket | DT.questionMark).map(_ => ())

  // Looking ahead to distinguish a single slash from a double slash, and to recognize the start of a relativePathExpr.
  // See xgc:leading-lone-slash constraint.

  def slashOnlyPathExpr[_: P]: P[PathExpr] =
    P(DT.slash ~ !canStartRelativePathExpr).map {
      _ => SlashOnlyPathExpr
    }

  // See above. Note that the next token is not a slash, because 2 slashes together make up one token,
  // and because canStartRelativePathExpr implies that the next token cannot be a slash anyway.

  def pathExprStartingWithSingleSlash[_: P]: P[PathExpr] =
    P(DT.slash ~ &(canStartRelativePathExpr) ~ relativePathExpr).map {
      expr => PathExprStartingWithSingleSlash(expr)
    }

  def pathExprStartingWithDoubleSlash[_: P]: P[PathExpr] =
    P(DT.doubleSlash ~ relativePathExpr).map {
      expr => PathExprStartingWithDoubleSlash(expr)
    }

  def relativePathExpr[_: P]: P[RelativePathExpr] =
    P(stepExpr ~ ((DT.slash | DT.doubleSlash).! ~/ stepExpr).rep).map {
      case (firstExp, opExpPairs) =>
        RelativePathExpr(firstExp, opExpPairs.toIndexedSeq.map(kv => StepOp.parse(kv._1) -> kv._2))
    }

  // The 2 branches of a stepExpr are relatively easy to distinguish. Note that both branches may start with an EQName (or "keyword"), and other than that
  // only start with mutually exclusive tokens. The difference between the 2 branches is that an axisStep starting with an EQName only contains the EQName,
  // whereas a postfixExpr may start but may never end with an EQName. Each postfixExpr starting with an EQName is a function call or named function
  // reference. Two constraints (xgc:reserved-function-names and gn:parens) further help in recognizing function calls and named function references.

  // Hence, we first try the branch for postfixExpr, and try branch axisStep if the first one fails.

  def stepExpr[_: P]: P[StepExpr] =
    P(postfixExpr | axisStep)

  // The 2 branches of an axisStep are relatively easy to distinguish. A reverseAxisStep is easy to recognize.
  // A forwardAxisStep is easy to recognize if non-abbreviated, and otherwise it starts with a nodeTest, possibly
  // preceded by "@".

  // We first try the reverseAxisStep, and only then the forwardAxisStep, to make sure that nodeTests are only
  // tried if all other options (like non-abbreviated steps) do not apply. Note that the lookahead needed for
  // discarding reverseAxisStep is limited (2 tokens).

  def axisStep[_: P]: P[AxisStep] =
    P(reverseAxisStep | forwardAxisStep)

  def forwardAxisStep[_: P]: P[ForwardAxisStep] =
    P(forwardStep ~ predicate.rep).map {
      case (forwardStep, predicates) => ForwardAxisStep(forwardStep, predicates.toIndexedSeq)
    }

  def reverseAxisStep[_: P]: P[ReverseAxisStep] =
    P(reverseStep ~ predicate.rep).map {
      case (reverseStep, predicates) => ReverseAxisStep(reverseStep, predicates.toIndexedSeq)
    }

  def forwardStep[_: P]: P[ForwardStep] =
    P(nonAbbrevForwardStep | abbrevForwardStep)

  def abbrevForwardStep[_: P]: P[AbbrevForwardStep] =
    P(simpleAbbrevForwardStep | attributeAxisAbbrevForwardStep)

  def simpleAbbrevForwardStep[_: P]: P[SimpleAbbrevForwardStep] =
    P(nodeTest).map {
      nodeTest => SimpleAbbrevForwardStep(nodeTest)
    }

  def attributeAxisAbbrevForwardStep[_: P]: P[AttributeAxisAbbrevForwardStep] =
    P(DT.at ~ nodeTest).map {
      nodeTest => AttributeAxisAbbrevForwardStep(nodeTest)
    }

  def nonAbbrevForwardStep[_: P]: P[NonAbbrevForwardStep] =
    P(forwardAxis ~/ nodeTest).map {
      case (axis, nodeTest) => NonAbbrevForwardStep(axis, nodeTest)
    }

  def forwardAxis[_: P]: P[ForwardAxis] =
    P((NDT.childWord | NDT.descendantWord | NDT.attributeWord | NDT.selfWord | NDT.descendantOrSelfWord |
      NDT.followingSiblingWord | NDT.followingWord | NDT.namespaceWord).! ~ DT.doubleColon).map {

      case "child" => ForwardAxis.Child
      case "descendant" => ForwardAxis.Descendant
      case "attribute" => ForwardAxis.Attribute
      case "self" => ForwardAxis.Self
      case "descendant-or-self" => ForwardAxis.DescendantOrSelf
      case "following-sibling" => ForwardAxis.FollowingSibling
      case "following" => ForwardAxis.Following
      case "namespace" => ForwardAxis.Namespace
    }

  def reverseStep[_: P]: P[ReverseStep] =
    P(nonAbbrevReverseStep | abbrevReverseStep)

  def abbrevReverseStep[_: P]: P[AbbrevReverseStep.type] =
    P(DT.doubleDot).map(_ => AbbrevReverseStep)

  def nonAbbrevReverseStep[_: P]: P[NonAbbrevReverseStep] =
    P(reverseAxis ~/ nodeTest).map {
      case (axis, nodeTest) => NonAbbrevReverseStep(axis, nodeTest)
    }

  def reverseAxis[_: P]: P[ReverseAxis] =
    P((NDT.parentWord | NDT.ancestorWord | NDT.precedingSiblingWord | NDT.precedingWord | NDT.ancestorOrSelfWord).! ~ DT.doubleColon).map {
      case "parent" => ReverseAxis.Parent
      case "ancestor" => ReverseAxis.Ancestor
      case "preceding-sibling" => ReverseAxis.PrecedingSibling
      case "preceding" => ReverseAxis.Preceding
      case "ancestor-or-self" => ReverseAxis.AncestorOrSelf
    }

  // The 2 branches of a nodeTest are easy to distinguish, with limited lookahead.
  // We first try branch kindTest, which always starts with a "keyword". If that fails, we try the nameTest branch.

  def nodeTest[_: P]: P[NodeTest] =
    P(kindTest | nameTest)

  // The 2 branches of a nameTest are relatively easy to distinguish. A simpleNameTest is just an EQName, whereas a wildcard
  // always contains an asterisk. Also mind the remarks below.

  // As per the grammar specification, we first try the simpleNameTest branch, and then, if it fails, the wildcard branch.
  // This way a URI qualified name will be recognized before the "braced URI literal wildcard". Because of the ws:explicit
  // constraint a prefix wildcard should not be "hidden" by a QName as EQName, but to make sure a prefix
  // wildcard is recognized we look ahead.

  def nameTest[_: P]: P[NameTest] =
    P(simpleNameTest | wildcard)

  def simpleNameTest[_: P]: P[SimpleNameTest] =
    P(eqName ~ !DT.colonAsterisk).map {
      name => SimpleNameTest(name)
    }

  // See ws:explicit constraint.

  def wildcard[_: P]: P[Wildcard] =
    P(Wildcards.wildcard)

  def kindTest[_: P]: P[KindTest] =
    P(documentTest | elementTest | attributeTest | schemaElementTest | schemaAttributeTest | piTest | commentTest | textTest | namespaceNodeTest | anyKindTest)

  def documentTest[_: P]: P[DocumentTest] =
    P(simpleDocumentTest | documentTestContainingElementTest | documentTestContainingSchemaElementTest)

  def simpleDocumentTest[_: P]: P[SimpleDocumentTest.type] =
    P(NDT.documentNodeWord ~ DT.openParenthesis ~ DT.closeParenthesis).map(_ => SimpleDocumentTest)

  def documentTestContainingElementTest[_: P]: P[DocumentTestContainingElementTest] =
    P(NDT.documentNodeWord ~ DT.openParenthesis ~ elementTest ~ DT.closeParenthesis).map {
      elemTest => DocumentTestContainingElementTest(elemTest)
    }

  def documentTestContainingSchemaElementTest[_: P]: P[DocumentTestContainingSchemaElementTest] =
    P(NDT.documentNodeWord ~ DT.openParenthesis ~ schemaElementTest ~ DT.closeParenthesis).map {
      schemaElmTest => DocumentTestContainingSchemaElementTest(schemaElmTest)
    }

  def elementTest[_: P]: P[ElementTest] =
    P(anyElementTest | elementNameTest | elementNameAndTypeTest | nillableElementNameAndTypeTest | elementTypeTest | nillableElementTypeTest)

  // Losing some efficiency on parsing of element tests

  def anyElementTest[_: P]: P[AnyElementTest.type] =
    P(NDT.elementWord ~ DT.openParenthesis ~ DT.asterisk.? ~ DT.closeParenthesis).map(_ => AnyElementTest)

  def elementNameTest[_: P]: P[ElementNameTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ eqName ~ DT.closeParenthesis).map {
      name => ElementNameTest(name)
    }

  def elementNameAndTypeTest[_: P]: P[ElementNameAndTypeTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ eqName ~ DT.comma ~ eqName ~ DT.closeParenthesis).map {
      case (name, tpe) => ElementNameAndTypeTest(name, tpe)
    }

  def nillableElementNameAndTypeTest[_: P]: P[NillableElementNameAndTypeTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ eqName ~ DT.comma ~ eqName ~ DT.questionMark ~ DT.closeParenthesis).map {
      case (name, tpe) => NillableElementNameAndTypeTest(name, tpe)
    }

  def elementTypeTest[_: P]: P[ElementTypeTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.comma ~ eqName ~ DT.closeParenthesis).map {
      tpe => ElementTypeTest(tpe)
    }

  def nillableElementTypeTest[_: P]: P[NillableElementTypeTest] =
    P(NDT.elementWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.comma ~ eqName ~ DT.questionMark ~ DT.closeParenthesis).map {
      tpe => NillableElementTypeTest(tpe)
    }

  def attributeTest[_: P]: P[AttributeTest] =
    P(anyAttributeTest | attributeNameTest | attributeNameAndTypeTest | attributeTypeTest)

  // Losing some efficiency on parsing of attribute tests

  def anyAttributeTest[_: P]: P[AnyAttributeTest.type] =
    P(NDT.attributeWord ~ DT.openParenthesis ~ DT.asterisk.? ~ DT.closeParenthesis).map(_ => AnyAttributeTest)

  def attributeNameTest[_: P]: P[AttributeNameTest] =
    P(NDT.attributeWord ~ DT.openParenthesis ~ eqName ~ DT.closeParenthesis).map {
      name => AttributeNameTest(name)
    }

  def attributeNameAndTypeTest[_: P]: P[AttributeNameAndTypeTest] =
    P(NDT.attributeWord ~ DT.openParenthesis ~ eqName ~ DT.comma ~ eqName ~ DT.closeParenthesis).map {
      case (name, tpe) => AttributeNameAndTypeTest(name, tpe)
    }

  def attributeTypeTest[_: P]: P[AttributeTypeTest] =
    P(NDT.attributeWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.comma ~ eqName ~ DT.closeParenthesis).map {
      tpe => AttributeTypeTest(tpe)
    }

  def schemaElementTest[_: P]: P[SchemaElementTest] =
    P(NDT.schemaElementWord ~ DT.openParenthesis ~ eqName ~ DT.closeParenthesis).map {
      name => SchemaElementTest(name)
    }

  def schemaAttributeTest[_: P]: P[SchemaAttributeTest] =
    P(NDT.schemaAttributeWord ~ DT.openParenthesis ~ eqName ~ DT.closeParenthesis).map {
      name => SchemaAttributeTest(name)
    }

  def piTest[_: P]: P[PITest] =
    P(simplePiTest | targetPiTest | dataPiTest)

  def simplePiTest[_: P]: P[SimplePITest.type] =
    P(NDT.processingInstructionWord ~ DT.openParenthesis ~ DT.closeParenthesis).map(_ => SimplePITest)

  def targetPiTest[_: P]: P[TargetPITest] =
    P(NDT.processingInstructionWord ~ DT.openParenthesis ~ ncName ~ DT.closeParenthesis).map {
      name => TargetPITest(name)
    }

  def dataPiTest[_: P]: P[DataPITest] =
    P(NDT.processingInstructionWord ~ DT.openParenthesis ~ stringLiteral ~ DT.closeParenthesis).map {
      stringLit => DataPITest(stringLit)
    }

  def commentTest[_: P]: P[CommentTest.type] =
    P(NDT.commentWord ~ DT.openParenthesis ~ DT.closeParenthesis).map(_ => CommentTest)

  def textTest[_: P]: P[TextTest.type] =
    P(NDT.textWord ~ DT.openParenthesis ~ DT.closeParenthesis).map(_ => TextTest)

  def namespaceNodeTest[_: P]: P[NamespaceNodeTest.type] =
    P(NDT.namespaceNodeWord ~ DT.openParenthesis ~ DT.closeParenthesis).map(_ => NamespaceNodeTest)

  def anyKindTest[_: P]: P[AnyKindTest.type] =
    P(NDT.nodeWord ~ DT.openParenthesis ~ DT.closeParenthesis).map(_ => AnyKindTest)

  // Postfix expressions

  def postfixExpr[_: P]: P[PostfixExpr] =
    P(primaryExpr ~ (predicate | argumentList | lookup).rep).map {
      case (primaryExp, postfixes) => PostfixExpr(primaryExp, postfixes.toIndexedSeq)
    }

  def argumentList[_: P]: P[ArgumentList] =
    P(DT.openParenthesis ~ argument.rep(sep = DT.comma) ~ DT.closeParenthesis).map {
      args => ArgumentList(NonEmptyVector.fromVectorUnsafe(args.toVector))
    }

  def argument[_: P]: P[Argument] =
    P(argumentPlaceholder | exprSingleArgument)

  def argumentPlaceholder[_: P]: P[ArgumentPlaceholder.type] =
    P(DT.questionMark).map(_ => ArgumentPlaceholder)

  def exprSingleArgument[_: P]: P[ExprSingleArgument] =
    P(exprSingle).map {
      exp => ExprSingleArgument(exp)
    }

  def lookup[_: P]: P[PostfixLookup] =
    P(DT.questionMark ~ keySpecifier).map {
      keySpec => PostfixLookup(keySpec)
    }

  def keySpecifier[_: P]: P[KeySpecifier] =
    P(ncName | integerLiteral | DT.asterisk.! | parenthesizedExpr).map {
      case nm: NCName => NamedKeySpecifier(nm)
      case intLit: IntegerLiteral => PositionalKeySpecifier(intLit)
      case "*" => WildcardKeySpecifier
      case exp: ParenthesizedExpr => ParenthesizedExprKeySpecifier(exp)
    }

  def paramList[_: P]: P[ParamList] =
    P(param.rep(min = 1, sep = DT.comma)).map {
      pars => ParamList(pars.toIndexedSeq)
    }

  def param[_: P]: P[Param] =
    P(DT.dollar ~ eqName ~ (NDT.asWord ~ sequenceType).?).map {
      case (name, tpeOption) => Param(name, tpeOption.map(t => TypeDeclaration(t)))
    }

  def predicate[_: P]: P[Predicate] =
    P(DT.openBracket ~ expr ~ DT.closeBracket).map {
      exp => Predicate(exp)
    }

  // Primary expressions

  // The branches of a primaryExpr are relatively easy to distinguish. See above.

  def primaryExpr[_: P]: P[PrimaryExpr] =
    P(literal | varRef | parenthesizedExpr | contextItemExpr | functionCall | functionItemExpr |
      mapConstructor | arrayConstructor | unaryLookup)

  def literal[_: P]: P[Literal] =
    P(stringLiteral | numericLiteral)

  // Using the StringLiterals.stringLiteral parser, etc.

  def stringLiteral[_: P]: P[StringLiteral] =
    P(DT.stringLiteral)

  def numericLiteral[_: P]: P[NumericLiteral] =
    P(NDT.numericLiteral)

  def integerLiteral[_: P]: P[IntegerLiteral] =
    P(NDT.integerLiteral)

  def varRef[_: P]: P[VarRef] =
    P(DT.dollar ~ eqName).map {
      name => VarRef(name)
    }

  def parenthesizedExpr[_: P]: P[ParenthesizedExpr] =
    P(DT.openParenthesis ~ expr.? ~ DT.closeParenthesis).map {
      expOption => ParenthesizedExpr(expOption)
    }

  def contextItemExpr[_: P]: P[ContextItemExpr.type] =
    P(DT.dot).map(_ => ContextItemExpr)

  // See xgc:reserved-function-names
  // TODO gn:parens. This becomes important once we support comments.

  def functionCall[_: P]: P[FunctionCall] =
    P(eqName.filter(nm => !ReservedFunctionNames.contains(nm)) ~ argumentList).map {
      case (name, argList) => FunctionCall(name, argList)
    }

  def functionItemExpr[_: P]: P[FunctionItemExpr] =
    P(namedFunctionRef | inlineFunctionExpr)

  // See xgc:reserved-function-names

  def namedFunctionRef[_: P]: P[NamedFunctionRef] =
    P(eqName.filter(nm => !ReservedFunctionNames.contains(nm)) ~ DT.hash ~ integerLiteral).map {
      case (name, arity) => NamedFunctionRef(name, arity.value)
    }

  def inlineFunctionExpr[_: P]: P[InlineFunctionExpr] =
    P(NDT.functionWord ~ DT.openParenthesis ~ paramList.? ~ DT.closeParenthesis ~ (NDT.asWord ~ sequenceType).? ~ enclosedExpr).map {
      case (parListOption, resultTpeOption, body) =>
        InlineFunctionExpr(parListOption, resultTpeOption, body)
    }

  def mapConstructor[_: P]: P[MapConstructor] =
    P(NDT.mapWord ~ DT.openBrace ~/ mapConstructorEntry.rep(sep = DT.comma) ~ DT.closeBrace).map {
      entries => MapConstructor(entries.toIndexedSeq)
    }

  def mapConstructorEntry[_: P]: P[MapConstructorEntry] =
    P(exprSingle ~ DT.colon ~ exprSingle).map {
      case (k, v) => MapConstructorEntry(k, v)
    }

  def arrayConstructor[_: P]: P[ArrayConstructor] =
    P(squareArrayConstructor | curlyArrayConstructor)

  def squareArrayConstructor[_: P]: P[SquareArrayConstructor] =
    P(DT.openBracket ~ exprSingle.rep(sep = DT.comma) ~ DT.closeBracket).map {
      members => SquareArrayConstructor(members.toIndexedSeq)
    }

  def curlyArrayConstructor[_: P]: P[CurlyArrayConstructor] =
    P(NDT.arrayWord ~ enclosedExpr).map {
      exp => CurlyArrayConstructor(exp)
    }

  def unaryLookup[_: P]: P[UnaryLookup] =
    P(DT.questionMark ~ keySpecifier).map {
      keySpec => UnaryLookup(keySpec)
    }

  // Types

  def sequenceType[_: P]: P[SequenceType] =
    P(emptySequenceType | nonEmptySequenceType)

  def emptySequenceType[_: P]: P[EmptySequenceType.type] =
    P(NDT.emptySequenceWord ~ DT.openParenthesis ~ DT.closeParenthesis).map(_ => EmptySequenceType)

  // TODO xgc:occurrence-indicators

  def nonEmptySequenceType[_: P]: P[SequenceType] =
    P(itemType ~ (DT.questionMark | DT.asterisk | DT.plus).!.?).map {
      case (tpe, None) => ExactlyOneSequenceType(tpe)
      case (tpe, Some("?")) => ZeroOrOneSequenceType(tpe)
      case (tpe, Some("*")) => ZeroOrMoreSequenceType(tpe)
      case (tpe, Some("+")) => OneOrMoreSequenceType(tpe)
      case _ => EmptySequenceType
    }

  def itemType[_: P]: P[ItemType] =
    P(kindTestItemType | anyItemType | anyFunctionTest | typedFunctionTest | atomicOrUnionType | parenthesizedItemType | mapTest | arrayTest)

  def kindTestItemType[_: P]: P[KindTestItemType] =
    P(kindTest).map {
      kindTst => KindTestItemType(kindTst)
    }

  def anyItemType[_: P]: P[AnyItemType.type] =
    P(NDT.itemWord ~ DT.openParenthesis ~ DT.closeParenthesis).map(_ => AnyItemType)

  def anyFunctionTest[_: P]: P[AnyFunctionTest.type] =
    P(NDT.functionWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.closeParenthesis).map(_ => AnyFunctionTest)

  def typedFunctionTest[_: P]: P[TypedFunctionTest] =
    P(NDT.functionWord ~ DT.openParenthesis ~ sequenceType.rep(sep = DT.comma) ~ DT.closeParenthesis ~ NDT.asWord ~ sequenceType).map {
      case (parTpes, resultTpe) => TypedFunctionTest(parTpes.toIndexedSeq, resultTpe)
    }

  def atomicOrUnionType[_: P]: P[AtomicOrUnionType] =
    P(eqName).map {
      tpe => AtomicOrUnionType(tpe)
    }

  def parenthesizedItemType[_: P]: P[ParenthesizedItemType] =
    P(DT.openParenthesis ~ itemType ~ DT.closeParenthesis).map {
      tpe => ParenthesizedItemType(tpe)
    }

  def mapTest[_: P]: P[MapTest] =
    P(anyMapTest | typedMapTest)

  def anyMapTest[_: P]: P[AnyMapTest.type] =
    P(NDT.mapWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.closeParenthesis).map {
      _ => AnyMapTest
    }

  def typedMapTest[_: P]: P[TypedMapTest] =
    P(NDT.mapWord ~ DT.openParenthesis ~ atomicOrUnionType ~ DT.comma ~ sequenceType ~ DT.closeParenthesis).map {
      case (kt, vt) => TypedMapTest(kt, vt)
    }

  def arrayTest[_: P]: P[ArrayTest] =
    P(anyArrayTest | typedArrayTest)

  def anyArrayTest[_: P]: P[AnyArrayTest.type] =
    P(NDT.arrayWord ~ DT.openParenthesis ~ DT.asterisk ~ DT.closeParenthesis).map {
      _ => AnyArrayTest
    }

  def typedArrayTest[_: P]: P[TypedArrayTest] =
    P(NDT.arrayWord ~ DT.openParenthesis ~ sequenceType ~ DT.closeParenthesis).map {
      et => TypedArrayTest(et)
    }

  def singleType[_: P]: P[SingleType] =
    P(eqName ~ DT.questionMark.!.?).map {
      case (tpe, None) => NonEmptySingleType(tpe)
      case (tpe, Some(_)) => PotentiallyEmptySingleType(tpe)
    }

  // Names (EQNames, NCNames etc.)
  // Using the NCNames.ncName and EQNames.eqName parsers

  private def ncName[_: P]: P[NCName] = P(NCNames.ncName)

  private def eqName[_: P]: P[EQName] = P(EQNames.eqName)

  // Operators etc.

  def comp[_: P]: P[Comp] =
    P(valueComp | generalComp | nodeComp)

  def valueComp[_: P]: P[ValueComp] =
    P((NDT.eqWord | NDT.neWord | NDT.ltWord | NDT.leWord | NDT.gtWord | NDT.geWord).!).map(s => ValueComp.parse(s))

  def generalComp[_: P]: P[GeneralComp] =
    P((DT.equals | DT.notEquals | DT.lessThan | DT.lessThanOrEqual |
      DT.greaterThan | DT.greaterThanOrEqual).!).map(s => GeneralComp.parse(s))

  def nodeComp[_: P]: P[NodeComp] =
    P((NDT.isWord | DT.precedes | DT.follows).!).map(s => NodeComp.parse(s))

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
