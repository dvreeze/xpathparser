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

import eu.cdevreeze.xpathparser.ast.DecimalLiteral
import eu.cdevreeze.xpathparser.ast.DoubleLiteral
import eu.cdevreeze.xpathparser.ast.EQName
import eu.cdevreeze.xpathparser.ast.IntegerLiteral
import eu.cdevreeze.xpathparser.ast.NCName
import eu.cdevreeze.xpathparser.ast.NumericLiteral

/**
 * Non-delimiting terminal symbols. No whitespace is skipped during this tokenization.
 * Lookahead is applied when needed to distinguish between different terminal symbols starting with
 * the same characters.
 *
 * @author Chris de Vreeze
 */
object NonDelimitingTerminals {
  import fastparse.all._

  // Numeric literals

  val numericLiteral: P[NumericLiteral] =
    P(integerLiteral | decimalLiteral | doubleLiteral)

  val integerLiteral: P[IntegerLiteral] =
    P(CharsWhileIn("0123456789").! ~ !CharIn(".", "e", "E")) filter (v => isIntegerLiteral(v)) map { v =>
      IntegerLiteral(BigInt(v))
    }

  val decimalLiteral: P[DecimalLiteral] =
    P(CharsWhileIn("0123456789.").! ~ !CharIn("e", "E")) filter (v => isDecimalLiteral(v)) map { v =>
      DecimalLiteral(BigDecimal(v))
    }

  val doubleLiteral: P[DoubleLiteral] =
    P(CharsWhileIn("0123456789.eE+-").!) filter (v => isDoubleLiteral(v)) map { v =>
      DoubleLiteral(v.toDouble)
    }

  // NCNames and EQNames

  val ncName: P[NCName] = NCNames.ncName

  val eqName: P[EQName] = EQNames.eqName

  // "Keywords"

  val ancestorWord: P[Unit] = parseWord("ancestor")

  val ancestorOrSelfWord: P[Unit] = parseWord("ancestor-or-self")

  val andWord: P[Unit] = parseWord("and")

  val asWord: P[Unit] = parseWord("as")

  val attributeWord: P[Unit] = parseWord("attribute")

  val castWord: P[Unit] = parseWord("cast")

  val castableWord: P[Unit] = parseWord("castable")

  val childWord: P[Unit] = parseWord("child")

  val commentWord: P[Unit] = parseWord("comment")

  val descendantWord: P[Unit] = parseWord("descendant")

  val descendantOrSelfWord: P[Unit] = parseWord("descendant-or-self")

  val divWord: P[Unit] = parseWord("div")

  val documentNodeWord: P[Unit] = parseWord("document-node")

  val elementWord: P[Unit] = parseWord("element")

  val elseWord: P[Unit] = parseWord("else")

  val emptySequenceWord: P[Unit] = parseWord("empty-sequence")

  val eqWord: P[Unit] = parseWord("eq")

  val everyWord: P[Unit] = parseWord("every")

  val exceptWord: P[Unit] = parseWord("except")

  val followingWord: P[Unit] = parseWord("following")

  val followingSiblingWord: P[Unit] = parseWord("following-sibling")

  val forWord: P[Unit] = parseWord("for")

  val functionWord: P[Unit] = parseWord("function")

  val geWord: P[Unit] = parseWord("ge")

  val gtWord: P[Unit] = parseWord("gt")

  val idivWord: P[Unit] = parseWord("idiv")

  val ifWord: P[Unit] = parseWord("if")

  val inWord: P[Unit] = parseWord("in")

  val instanceWord: P[Unit] = parseWord("instance")

  val intersectWord: P[Unit] = parseWord("intersect")

  val isWord: P[Unit] = parseWord("is")

  val itemWord: P[Unit] = parseWord("item")

  val leWord: P[Unit] = parseWord("le")

  val letWord: P[Unit] = parseWord("let")

  val ltWord: P[Unit] = parseWord("lt")

  val modWord: P[Unit] = parseWord("mod")

  val namespaceWord: P[Unit] = parseWord("namespace")

  val namespaceNodeWord: P[Unit] = parseWord("namespace-node")

  val neWord: P[Unit] = parseWord("ne")

  val nodeWord: P[Unit] = parseWord("node")

  val ofWord: P[Unit] = parseWord("of")

  val orWord: P[Unit] = parseWord("or")

  val parentWord: P[Unit] = parseWord("parent")

  val precedingWord: P[Unit] = parseWord("preceding")

  val precedingSiblingWord: P[Unit] = parseWord("preceding-sibling")

  val processingInstructionWord: P[Unit] = parseWord("processing-instruction")

  val returnWord: P[Unit] = parseWord("return")

  val satisfiesWord: P[Unit] = parseWord("satisfies")

  val schemaAttributeWord: P[Unit] = parseWord("schema-attribute")

  val schemaElementWord: P[Unit] = parseWord("schema-element")

  val selfWord: P[Unit] = parseWord("self")

  val someWord: P[Unit] = parseWord("some")

  val textWord: P[Unit] = parseWord("text")

  val thenWord: P[Unit] = parseWord("then")

  val toWord: P[Unit] = parseWord("to")

  val treatWord: P[Unit] = parseWord("treat")

  val unionWord: P[Unit] = parseWord("union")

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

  // TODO Is this a correct symbol separator, or should we look at whitespace and comments instead?

  private val symbolSeparator: P[Unit] =
    P(End | CharPred(c => !NCName.canBePartOfNCName(c)))

  def parseWord(s: String): P[Unit] = {
    P(s ~ &(symbolSeparator))
  }
}
