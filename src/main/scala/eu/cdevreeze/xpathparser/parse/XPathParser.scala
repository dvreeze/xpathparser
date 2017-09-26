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
import eu.cdevreeze.xpathparser.ast.EQName
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

  object Names {
    import fastparse.all._

    // TODO Make more efficient

    val ncName: P[NCName] =
      P(CharPred(c => NCName.canBeStartOfNCName(c)).! ~ CharPred(c => NCName.canBePartOfNCName(c)).rep.!) map {
        case (s1, s2) => NCName(s1 + s2)
      }

    // We could change the order of the 2 branches below, but I'd rather explicitly use a small lookahead.

    val eqName: P[EQName] =
      P(!"Q{" ~ qName | uriQualifiedName)

    val qName: P[EQName.QName] =
      P(ncName ~ (":" ~ ncName).?) map {
        case (s1, s2Opt) =>
          if (s2Opt.isEmpty) {
            EQName.QName.parse(s1.name)
          } else {
            EQName.QName.parse(s1.name + ":" + s2Opt.get.name)
          }
      }

    val uriQualifiedName: P[EQName.URIQualifiedName] =
      P("Q{" ~/ CharPred(c => isAllowedUriChar(c)).rep.! ~ "}" ~ ncName) map {
        case (uri, localPart) =>
          EQName.URIQualifiedName.parse("Q{" + uri + "}" + localPart.name)
      }

    private def isAllowedUriChar(c: Char): Boolean = {
      // Just guessing!

      !java.lang.Character.isWhitespace(c) && (c != '{') && (c != '}')
    }
  }

  object StringLiterals {
    import fastparse.all._

    val stringLiteral: P[StringLiteral] =
      P(aposStringLiteral | quoteStringLiteral)

    // TODO Make more efficient

    private val aposStringLiteral: P[StringLiteral] =
      P("'" ~/ (escapeApos | nonEscapedCharInAposStringLiteral).rep.! ~ "'") map { v =>
        // Why do we still need the "unescaping" here?

        StringLiteral(v.replace("''", "'"))
      }

    private val quoteStringLiteral: P[StringLiteral] =
      P("\"" ~/ (escapeQuote | nonEscapedCharInQuoteStringLiteral).rep.! ~ "\"") map { v =>
        // Why do we still need the "unescaping" here?

        StringLiteral(v.replace("\"\"", "\""))
      }

    private val escapeApos: P[String] =
      P("'".rep(exactly = 2).!) map (_.substring(0, 1).ensuring(_.size == 1))

    private val nonEscapedCharInAposStringLiteral: P[String] =
      P(CharPred(_ != '\'').!) map (_.ensuring(_.size == 1))

    private val escapeQuote: P[String] =
      P("\"".rep(exactly = 2).!) map (_.substring(0, 1).ensuring(_.size == 1))

    private val nonEscapedCharInQuoteStringLiteral: P[String] =
      P(CharPred(_ != '"').!) map (_.ensuring(_.size == 1))
  }

  /**
   * "Tokenization" (for operator symbols), to distinguish between operator symbols that start with the same character, etc.
   * No whitespace is skipped inside these symbol parsers.
   *
   * TODO Tokens for "keywords".
   */
  object Tokens {
    import fastparse.all._

    // Single and double slashes

    val singleSlash: P[Unit] =
      P("/" ~ !"/")

    val doubleSlash: P[Unit] =
      P("//")

    // Exclamation marks and not-equals symbol

    val exclamationMark: P[Unit] =
      P("!" ~ !"=")

    val notEqualsSymbol: P[Unit] =
      P("!=")

    // String concatenation and union symbols

    val unionSymbol: P[Unit] =
      P("|" ~ !"|")

    val stringConcatSymbol: P[Unit] =
      P("||")

    // Symbols starting with less-than character

    val lessThanSymbol: P[Unit] =
      P("<" ~ !("=" | "<"))

    val lessThanEqualsSymbol: P[Unit] =
      P("<=")

    val precedesSymbol: P[Unit] =
      P("<<")

    // Symbols starting with greater-than character

    val greaterThanSymbol: P[Unit] =
      P(">" ~ !("=" | ">"))

    val greaterThanEqualsSymbol: P[Unit] =
      P(">=")

    val followsSymbol: P[Unit] =
      P(">>")

    // Single and double dots

    val singleDot: P[Unit] =
      P("." ~ !".")

    val doubleDot: P[Unit] =
      P("..")

    // Single and double colons and assignment symbol

    val singleColon: P[Unit] =
      P(":" ~ !(":" | "="))

    val doubleColon: P[Unit] =
      P("::")

    val assignmentSymbol: P[Unit] =
      P(":=")
  }

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
    P(exprSingle.rep(min = 1, sep = ",")) map {
      case (exprs) => Expr(exprs.toIndexedSeq)
    }

  private val enclosedExpr: P[EnclosedExpr] =
    P("{" ~ expr ~ "}") map {
      case exp => EnclosedExpr(exp)
    }

  // The branches of exprSingle are easy to distinguish. All but one start with a different keyword.
  // Anything else must be an orExpr (if parsing succeeds).

  private val exprSingle: P[ExprSingle] =
    P(forExpr | letExpr | quantifiedExpr | ifExpr | orExpr)

  private val forExpr: P[ForExpr] =
    P("for" ~/ simpleForBinding.rep(min = 1, sep = ",") ~ "return" ~ exprSingle) map {
      case (bindings, returnExp) => ForExpr(bindings.toIndexedSeq, returnExp)
    }

  private val simpleForBinding: P[SimpleForBinding] =
    P("$" ~ eqName ~ "in" ~ exprSingle) map {
      case (eqn, exp) => SimpleForBinding(eqn, exp)
    }

  private val letExpr: P[LetExpr] =
    P("let" ~/ simpleLetBinding.rep(min = 1, sep = ",") ~ "return" ~ exprSingle) map {
      case (bindings, returnExp) => LetExpr(bindings.toIndexedSeq, returnExp)
    }

  private val simpleLetBinding: P[SimpleLetBinding] =
    P("$" ~ eqName ~ Tokens.assignmentSymbol ~ exprSingle) map {
      case (eqn, exp) => SimpleLetBinding(eqn, exp)
    }

  private val quantifiedExpr: P[QuantifiedExpr] =
    P(StringIn("some", "every").! ~/ simpleBindingInQuantifiedExpr.rep(min = 1, sep = ",") ~ "satisfies" ~ exprSingle) map {
      case (quant, bindings, satisfiesExp) => QuantifiedExpr(Quantifier.parse(quant), bindings.toIndexedSeq, satisfiesExp)
    }

  private val simpleBindingInQuantifiedExpr: P[SimpleBindingInQuantifiedExpr] =
    P("$" ~ eqName ~ "in" ~ exprSingle) map {
      case (eqn, exp) => SimpleBindingInQuantifiedExpr(eqn, exp)
    }

  private val ifExpr: P[IfExpr] =
    P("if" ~/ "(" ~ expr ~ ")" ~ "then" ~ exprSingle ~ "else" ~ exprSingle) map {
      case (e1, e2, e3) => IfExpr(e1, e2, e3)
    }

  private val orExpr: P[OrExpr] =
    P(andExpr.rep(min = 1, sep = "or" ~/ Pass)) map {
      case exps => OrExpr(exps.toIndexedSeq)
    }

  private val andExpr: P[AndExpr] =
    P(comparisonExpr.rep(min = 1, sep = "and" ~/ Pass)) map {
      case exps => AndExpr(exps.toIndexedSeq)
    }

  private val comparisonExpr: P[ComparisonExpr] =
    P(stringConcatExpr ~ ((valueComp | generalComp | nodeComp) ~/ stringConcatExpr).?) map {
      case (expr1, Some((op, expr2))) => CompoundComparisonExpr(expr1, op, expr2)
      case (expr, None)               => SimpleComparisonExpr(expr)
    }

  private val stringConcatExpr: P[StringConcatExpr] =
    P(rangeExpr.rep(min = 1, sep = Tokens.stringConcatSymbol ~/ Pass)) map {
      case exps => StringConcatExpr(exps.toIndexedSeq)
    }

  private val rangeExpr: P[RangeExpr] =
    P(additiveExpr ~ ("to" ~/ additiveExpr).?) map {
      case (additiveExp1, Some(additiveExp2)) => CompoundRangeExpr(additiveExp1, additiveExp2)
      case (additiveExp, None)                => SimpleRangeExpr(additiveExp)
    }

  private val additiveExpr: P[AdditiveExpr] =
    P(multiplicativeExpr ~ (StringIn("+", "-").! ~/ additiveExpr).?) map {
      case (expr, None)            => SimpleAdditiveExpr(expr)
      case (expr, Some(opAndExpr)) => CompoundAdditiveExpr(expr, AdditionOp.parse(opAndExpr._1), opAndExpr._2)
    }

  private val multiplicativeExpr: P[MultiplicativeExpr] =
    P(unionExpr ~ (StringIn("*", "div", "idiv", "mod").! ~/ multiplicativeExpr).?) map {
      case (expr, None)            => SimpleMultiplicativeExpr(expr)
      case (expr, Some(opAndExpr)) => CompoundMultiplicativeExpr(expr, MultiplicativeOp.parse(opAndExpr._1), opAndExpr._2)
    }

  private val unionExpr: P[UnionExpr] =
    P(intersectExceptExpr ~ (("union" | Tokens.unionSymbol) ~/ intersectExceptExpr).rep) map {
      case (expr, exprSeq) => UnionExpr(expr +: exprSeq.toIndexedSeq)
    }

  private val intersectExceptExpr: P[IntersectExceptExpr] =
    P(instanceOfExpr ~ (StringIn("intersect", "except").! ~/ intersectExceptExpr).?) map {
      case (expr, None)            => SimpleIntersectExceptExpr(expr)
      case (expr, Some(opAndExpr)) => CompoundIntersectExceptExpr(expr, IntersectExceptOp.parse(opAndExpr._1), opAndExpr._2)
    }

  private val instanceOfExpr: P[InstanceOfExpr] =
    P(treatExpr ~ ("instance" ~ "of" ~/ sequenceType).?) map {
      case (expr, tpeOption) => InstanceOfExpr(expr, tpeOption)
    }

  private val treatExpr: P[TreatExpr] =
    P(castableExpr ~ ("treat" ~ "as" ~/ sequenceType).?) map {
      case (expr, tpeOption) => TreatExpr(expr, tpeOption)
    }

  private val castableExpr: P[CastableExpr] =
    P(castExpr ~ ("castable" ~ "as" ~/ singleType).?) map {
      case (expr, tpeOption) => CastableExpr(expr, tpeOption)
    }

  private val castExpr: P[CastExpr] =
    P(unaryExpr ~ ("cast" ~ "as" ~/ singleType).?) map {
      case (expr, tpeOption) => CastExpr(expr, tpeOption)
    }

  private val unaryExpr: P[UnaryExpr] =
    P(StringIn("-", "+").!.rep ~ valueExpr) map {
      case (ops, expr) => UnaryExpr(ops.toIndexedSeq.map(op => UnaryOp.parse(op)), expr)
    }

  private val valueExpr: P[ValueExpr] =
    P(simpleMapExpr) map {
      case expr => ValueExpr(expr)
    }

  private val simpleMapExpr: P[SimpleMapExpr] =
    P(pathExpr.rep(min = 1, sep = Tokens.exclamationMark)) map {
      case exps => SimpleMapExpr(exps.toIndexedSeq)
    }

  // The branches of exprSingle are easy to distinguish, once we look ahead a bit, and mind constraint xgc:leading-lone-slash.
  // We can clearly distinguish among a single slash, a double slash, and a start of a relativePathExpr
  // (which cannot start with a slash, partly because an EQName cannot start with a slash).

  private val pathExpr: P[PathExpr] =
    P(slashOnlyPathExpr | pathExprStartingWithSingleSlash | pathExprStartingWithDoubleSlash | relativePathExpr)

  // Lookahead parsers

  private val canStartRelativePathExpr: P[Unit] =
    P(canStartAxisStep | canStartPostfixExpr)

  // The start of an axis step is easy to recognize, unless it is a nodeTest. The latter is a kindTest (easy to recognize),
  // wildcard or EQName. The EQName variant makes it harder to distinguish an axisStep from a postfixExpr.

  private val canStartAxisStep: P[Unit] =
    P(forwardAxis | reverseAxis | "@" | Tokens.doubleDot | nodeTest).map(_ => ())

  // A postfix expression starts with a (string or numeric) literal, dollar sign, (opening) parenthesis, dot,
  // NCName or URI-qualified name or the token "function". (Note that, like context items, decimal and double literals may start with dots.)

  private val canStartPostfixExpr: P[Unit] =
    P(literal | varRef | "(" | contextItemExpr | eqName | "function").map(_ => ())

  // Looking ahead to distinguish single slash from double slash, and to recognize start of relativePathExpr.
  // See xgc:leading-lone-slash constraint.

  private val slashOnlyPathExpr: P[PathExpr] =
    P(Tokens.singleSlash ~ !canStartRelativePathExpr) map {
      case _ => SlashOnlyPathExpr
    }

  // Looking ahead to distinguish single slash from double slash, and to recognize start of relativePathExpr.
  // See xgc:leading-lone-slash constraint. Note that canStartRelativePathExpr implies that the next token is not a slash!

  private val pathExprStartingWithSingleSlash: P[PathExpr] =
    P(Tokens.singleSlash ~ &(canStartRelativePathExpr) ~ relativePathExpr) map {
      case expr => PathExprStartingWithSingleSlash(expr)
    }

  private val pathExprStartingWithDoubleSlash: P[PathExpr] =
    P(Tokens.doubleSlash ~ relativePathExpr) map {
      case expr => PathExprStartingWithDoubleSlash(expr)
    }

  private val relativePathExpr: P[RelativePathExpr] =
    P(stepExpr ~ ((Tokens.singleSlash | Tokens.doubleSlash).! ~/ relativePathExpr).?) map {
      case (expr, None)            => SimpleRelativePathExpr(expr)
      case (expr, Some(opAndExpr)) => CompoundRelativePathExpr(expr, StepOp.parse(opAndExpr._1), opAndExpr._2)
    }

  // TODO Disambiguate the 2 branches of a stepExpr. Note that both branches may start with an EQName.
  // The difference is that an axisStep starting with an EQName only contains the EQName, whereas a postfixExpr may start but may never
  // end with an EQName.

  private val stepExpr: P[StepExpr] =
    P(postfixExpr | axisStep)

  // The 2 branches of an axisStep are relatively easy to distinguish. A reverseAxisStep is easy to recognize.
  // A forwardAxisStep is easy to recognize if non-abbreviated, and otherwise it starts with a nodeTest, possibly
  // preceded by "@".

  private val axisStep: P[AxisStep] =
    P(forwardAxisStep | reverseAxisStep)

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
    P("@" ~ nodeTest) map {
      case nodeTest => AttributeAxisAbbrevForwardStep(nodeTest)
    }

  private val nonAbbrevForwardStep: P[NonAbbrevForwardStep] =
    P(forwardAxis ~/ nodeTest) map {
      case (axis, nodeTest) => NonAbbrevForwardStep(axis, nodeTest)
    }

  private val forwardAxis: P[ForwardAxis] =
    P(StringIn("child", "descendant", "attribute", "self", "descendant-or-self",
      "following-sibling", "following", "namespace").! ~ Tokens.doubleColon) map {

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
    P(Tokens.doubleDot) map (_ => AbbrevReverseStep)

  private val nonAbbrevReverseStep: P[NonAbbrevReverseStep] =
    P(reverseAxis ~/ nodeTest) map {
      case (axis, nodeTest) => NonAbbrevReverseStep(axis, nodeTest)
    }

  private val reverseAxis: P[ReverseAxis] =
    P(StringIn("parent", "ancestor", "preceding-sibling", "preceding", "ancestor-or-self").! ~ Tokens.doubleColon) map {
      case "parent"            => ReverseAxis.Parent
      case "ancestor"          => ReverseAxis.Ancestor
      case "preceding-sibling" => ReverseAxis.PrecedingSibling
      case "preceding"         => ReverseAxis.Preceding
      case "ancestor-or-self"  => ReverseAxis.AncestorOrSelf
    }

  // The 2 branches of a nodeTest are easy to distinguish.

  private val nodeTest: P[NodeTest] =
    P(kindTest | nameTest)

  // The 2 branches of a nameTest are easy to distinguish.

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
    P("document-node" ~ "(" ~ ")") map (_ => SimpleDocumentTest)

  private val documentTestContainingElementTest: P[DocumentTestContainingElementTest] =
    P("document-node" ~ "(" ~ elementTest ~ ")") map {
      case elemTest => DocumentTestContainingElementTest(elemTest)
    }

  private val documentTestContainingSchemaElementTest: P[DocumentTestContainingSchemaElementTest] =
    P("document-node" ~ "(" ~ schemaElementTest ~ ")") map {
      case schemaElmTest => DocumentTestContainingSchemaElementTest(schemaElmTest)
    }

  private val elementTest: P[ElementTest] =
    P(anyElementTest | elementNameTest | elementNameAndTypeTest | nillableElementNameAndTypeTest | elementTypeTest | nillableElementTypeTest)

  // Losing some efficiency on parsing of element tests

  private val anyElementTest: P[AnyElementTest.type] =
    P("element" ~ "(" ~ "*".? ~ ")") map (_ => AnyElementTest)

  private val elementNameTest: P[ElementNameTest] =
    P("element" ~ "(" ~ eqName ~ ")") map {
      case name => ElementNameTest(name)
    }

  private val elementNameAndTypeTest: P[ElementNameAndTypeTest] =
    P("element" ~ "(" ~ eqName ~ "," ~ eqName ~ ")") map {
      case (name, tpe) => ElementNameAndTypeTest(name, tpe)
    }

  private val nillableElementNameAndTypeTest: P[NillableElementNameAndTypeTest] =
    P("element" ~ "(" ~ eqName ~ "," ~ eqName ~ "?" ~ ")") map {
      case (name, tpe) => NillableElementNameAndTypeTest(name, tpe)
    }

  private val elementTypeTest: P[ElementTypeTest] =
    P("element" ~ "(" ~ "*" ~ "," ~ eqName ~ ")") map {
      case tpe => ElementTypeTest(tpe)
    }

  private val nillableElementTypeTest: P[NillableElementTypeTest] =
    P("element" ~ "(" ~ "*" ~ "," ~ eqName ~ "?" ~ ")") map {
      case tpe => NillableElementTypeTest(tpe)
    }

  private val attributeTest: P[AttributeTest] =
    P(anyAttributeTest | attributeNameTest | attributeNameAndTypeTest | attributeTypeTest)

  // Losing some efficiency on parsing of attribute tests

  private val anyAttributeTest: P[AnyAttributeTest.type] =
    P("attribute" ~ "(" ~ "*".? ~ ")") map (_ => AnyAttributeTest)

  private val attributeNameTest: P[AttributeNameTest] =
    P("attribute" ~ "(" ~ eqName ~ ")") map {
      case name => AttributeNameTest(name)
    }

  private val attributeNameAndTypeTest: P[AttributeNameAndTypeTest] =
    P("attribute" ~ "(" ~ eqName ~ "," ~ eqName ~ ")") map {
      case (name, tpe) => AttributeNameAndTypeTest(name, tpe)
    }

  private val attributeTypeTest: P[AttributeTypeTest] =
    P("attribute" ~ "(" ~ "*" ~ "," ~ eqName ~ ")") map {
      case tpe => AttributeTypeTest(tpe)
    }

  private val schemaElementTest: P[SchemaElementTest] =
    P("schema-element" ~ "(" ~ eqName ~ ")") map {
      case name => SchemaElementTest(name)
    }

  private val schemaAttributeTest: P[SchemaAttributeTest] =
    P("schema-attribute" ~ "(" ~ eqName ~ ")") map {
      case name => SchemaAttributeTest(name)
    }

  private val piTest: P[PITest] =
    P(simplePiTest | targetPiTest | dataPiTest)

  private val simplePiTest: P[SimplePITest.type] =
    P("processing-instruction" ~ "(" ~ ")") map (_ => SimplePITest)

  private val targetPiTest: P[TargetPITest] =
    P("processing-instruction" ~ "(" ~ ncName ~ ")") map {
      case name => TargetPITest(name)
    }

  private val dataPiTest: P[DataPITest] =
    P("processing-instruction" ~ "(" ~ stringLiteral ~ ")") map {
      case stringLit => DataPITest(stringLit)
    }

  private val commentTest: P[CommentTest.type] =
    P("comment" ~ "(" ~ ")") map (_ => CommentTest)

  private val textTest: P[TextTest.type] =
    P("text" ~ "(" ~ ")") map (_ => TextTest)

  private val namespaceNodeTest: P[NamespaceNodeTest.type] =
    P("namespace-node" ~ "(" ~ ")") map (_ => NamespaceNodeTest)

  private val anyKindTest: P[AnyKindTest.type] =
    P("node" ~ "(" ~ ")") map (_ => AnyKindTest)

  private val postfixExpr: P[PostfixExpr] =
    P(primaryExpr ~ (predicate | argumentList).rep) map {
      case (primaryExp, predicateOrArgumentListSeq) => PostfixExpr(primaryExp, predicateOrArgumentListSeq.toIndexedSeq)
    }

  private val argumentList: P[ArgumentList] =
    P("(" ~ argument.rep(sep = ",") ~ ")") map {
      case args => ArgumentList(args.toIndexedSeq)
    }

  private val argument: P[Argument] =
    P(argumentPlaceholder | exprSingleArgument)

  private val argumentPlaceholder: P[ArgumentPlaceholder.type] =
    P("?") map (_ => ArgumentPlaceholder)

  private val exprSingleArgument: P[ExprSingleArgument] =
    P(exprSingle) map {
      case exp => ExprSingleArgument(exp)
    }

  private val paramList: P[ParamList] =
    P(param.rep(min = 1, sep = ",")) map {
      case pars => ParamList(pars.toIndexedSeq)
    }

  private val param: P[Param] =
    P("$" ~ eqName ~ ("as" ~ sequenceType).?) map {
      case (name, tpeOption) => Param(name, tpeOption.map(t => TypeDeclaration(t)))
    }

  private val predicate: P[Predicate] =
    P("[" ~ expr ~ "]") map {
      case exp => Predicate(exp)
    }

  // Primary expressions

  // The branches of a primaryExpr are relatively easy to distinguish. See above.

  private val primaryExpr: P[PrimaryExpr] =
    P(literal | varRef | parenthesizedExpr | contextItemExpr | functionCall | functionItemExpr)

  private val literal: P[Literal] =
    P(stringLiteral | numericLiteral)

  // Using the StringLiterals.stringLiteral parser

  private val stringLiteral: P[StringLiteral] =
    P(StringLiterals.stringLiteral)

  private val numericLiteral: P[NumericLiteral] =
    P(integerLiteral | decimalLiteral | doubleLiteral)

  private val integerLiteral: P[IntegerLiteral] =
    P(CharsWhileIn("0123456789").!) filter (v => isIntegerLiteral(v)) map (v => IntegerLiteral(v.toInt))

  private val decimalLiteral: P[DecimalLiteral] =
    P(CharsWhileIn("0123456789.").!) filter (v => isDecimalLiteral(v)) map (v => DecimalLiteral(BigDecimal(v)))

  private val doubleLiteral: P[DoubleLiteral] =
    P(CharsWhileIn("0123456789.eE+-").!) filter (v => isDoubleLiteral(v)) map (v => DoubleLiteral(v.toDouble))

  private val varRef: P[VarRef] =
    P("$" ~ eqName) map {
      name => VarRef(name)
    }

  private val parenthesizedExpr: P[ParenthesizedExpr] =
    P("(" ~ expr.? ~ ")") map {
      case expOption => ParenthesizedExpr(expOption)
    }

  private val contextItemExpr: P[ContextItemExpr.type] =
    P(Tokens.singleDot) map (_ => ContextItemExpr)

  // See xgc:reserved-function-names
  // TODO gn:parens

  private val functionCall: P[FunctionCall] =
    P(eqName.filter(nm => !ReservedFunctionNames.contains(nm)) ~ argumentList) map {
      case (name, argList) => FunctionCall(name, argList)
    }

  private val functionItemExpr: P[FunctionItemExpr] =
    P(namedFunctionRef | inlineFunctionExpr)

  // See xgc:reserved-function-names

  private val namedFunctionRef: P[NamedFunctionRef] =
    P(eqName.filter(nm => !ReservedFunctionNames.contains(nm)) ~ "#" ~ integerLiteral) map {
      case (name, arity) => NamedFunctionRef(name, arity.value)
    }

  private val inlineFunctionExpr: P[InlineFunctionExpr] =
    P("function" ~ "(" ~ paramList.? ~ ")" ~ ("as" ~ sequenceType).? ~ enclosedExpr) map {
      case (parListOption, resultTpeOption, body) =>
        InlineFunctionExpr(parListOption, resultTpeOption, body)
    }

  // Types

  private val sequenceType: P[SequenceType] =
    P(emptySequenceType | nonEmptySequenceType)

  private val emptySequenceType: P[EmptySequenceType.type] =
    P("empty-sequence" ~ "(" ~ ")") map (_ => EmptySequenceType)

  // TODO xgc:occurrence-indicators

  private val nonEmptySequenceType: P[SequenceType] =
    P(itemType ~ ("?" | "*" | "+").!.?) map {
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
    P("item" ~ "(" ~ ")") map (_ => AnyItemType)

  private val anyFunctionTest: P[AnyFunctionTest.type] =
    P("function" ~ "(" ~ "*" ~ ")") map (_ => AnyFunctionTest)

  private val typedFunctionTest: P[TypedFunctionTest] =
    P("function" ~ "(" ~ sequenceType.rep(sep = ",") ~ ")" ~ "as" ~ sequenceType) map {
      case (parTpes, resultTpe) => TypedFunctionTest(parTpes.toIndexedSeq, resultTpe)
    }

  private val atomicOrUnionType: P[AtomicOrUnionType] =
    P(eqName) map {
      case tpe => AtomicOrUnionType(tpe)
    }

  private val parenthesizedItemType: P[ParenthesizedItemType] =
    P("(" ~ itemType ~ ")") map {
      case tpe => ParenthesizedItemType(tpe)
    }

  private val singleType: P[SingleType] =
    P(eqName ~ "?".!.?) map {
      case (tpe, None)    => NonEmptySingleType(tpe)
      case (tpe, Some(_)) => PotentiallyEmptySingleType(tpe)
    }

  // Names (EQNames, NCNames etc.)
  // Using the Names.ncName and Names.eqName parsers

  private val ncName: P[NCName] =
    P(Names.ncName)

  private val eqName: P[EQName] =
    P(Names.eqName)

  // Operators etc.

  private val valueComp: P[ValueComp] =
    P(("eq" | "ne" | "lt" | "le" | "gt" | "ge").!) map (s => ValueComp.parse(s))

  private val generalComp: P[GeneralComp] =
    P(("=" | Tokens.notEqualsSymbol | Tokens.lessThanSymbol | Tokens.lessThanEqualsSymbol |
      Tokens.greaterThanSymbol | Tokens.greaterThanEqualsSymbol).!) map (s => GeneralComp.parse(s))

  private val nodeComp: P[NodeComp] =
    P(("is" | Tokens.precedesSymbol | Tokens.followsSymbol).!) map (s => NodeComp.parse(s))

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
    s.startsWith("Q{") && s.endsWith("}*") && NCName.canBeNCName(s.drop(2).dropRight(2))
  }

  private def isNCNameCharOrColonOrStar(c: Char): Boolean = {
    NCName.canBePartOfNCName(c) || (c == ':') || (c == '*')
  }

  private def isNCNameCharOrBraceOrStar(c: Char): Boolean = {
    NCName.canBePartOfNCName(c) || (c == '{') || (c == '}') || (c == '*')
  }

  private def isIntegerLiteral(s: String): Boolean = {
    s.nonEmpty && s.forall(c => java.lang.Character.isDigit(c))
  }

  private def isDecimalLiteral(s: String): Boolean = {
    // Note that it is important to differentiate between decimal literals on the one hand
    // and context item expressions and abbreviated reverse steps on the other hand!

    s.nonEmpty && (s.count(_ == '.') == 1) &&
      s.exists(c => java.lang.Character.isDigit(c)) &&
      s.forall(c => java.lang.Character.isDigit(c) || (c == '.'))
  }

  private def isDoubleLiteral(s: String): Boolean = {
    val idx = s.indexWhere(c => (c == 'e') || (c == 'E'))

    (idx > 0) && {
      val base = s.substring(0, idx)
      val exp = s.substring(idx + 1)
      val expWithoutSign = if (exp.startsWith("+") || exp.startsWith("-")) exp.drop(1) else exp

      (isIntegerLiteral(base) || isDecimalLiteral(base)) && isIntegerLiteral(expWithoutSign)
    }
  }
}
