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
import fastparse.NoWhitespace._

/**
 * Non-delimiting terminal symbols. No whitespace is skipped during this tokenization.
 * Lookahead is applied when needed to distinguish between different terminal symbols starting with
 * the same characters.
 *
 * @author Chris de Vreeze
 */
object NonDelimitingTerminals {
  import fastparse._

  // Numeric literals

  def numericLiteral[_: P]: P[NumericLiteral] =
    P(integerLiteral | decimalLiteral | doubleLiteral)

  def integerLiteral[_: P]: P[IntegerLiteral] =
    P(CharsWhileIn("0123456789").! ~ !CharIn("eE", ".")) filter (v => isIntegerLiteral(v)) map { v =>
      IntegerLiteral(BigInt(v))
    }

  def decimalLiteral[_: P]: P[DecimalLiteral] =
    P(CharsWhileIn("0123456789.").! ~ !CharIn("eE")) filter (v => isDecimalLiteral(v)) map { v =>
      DecimalLiteral(BigDecimal(v))
    }

  def doubleLiteral[_: P]: P[DoubleLiteral] =
    P(CharsWhileIn("0123456789.eE+\\-").!) filter (v => isDoubleLiteral(v)) map { v =>
      DoubleLiteral(v.toDouble)
    }

  // NCNames and EQNames

  def ncName[_: P]: P[NCName] = NCNames.ncName

  def eqName[_: P]: P[EQName] = EQNames.eqName

  // "Keywords"

  def ancestorWord[_: P]: P[Unit] = parseWord("ancestor")

  def ancestorOrSelfWord[_: P]: P[Unit] = parseWord("ancestor-or-self")

  def andWord[_: P]: P[Unit] = parseWord("and")

  def arrayWord[_: P]: P[Unit] = parseWord("array")

  def asWord[_: P]: P[Unit] = parseWord("as")

  def attributeWord[_: P]: P[Unit] = parseWord("attribute")

  def castWord[_: P]: P[Unit] = parseWord("cast")

  def castableWord[_: P]: P[Unit] = parseWord("castable")

  def childWord[_: P]: P[Unit] = parseWord("child")

  def commentWord[_: P]: P[Unit] = parseWord("comment")

  def descendantWord[_: P]: P[Unit] = parseWord("descendant")

  def descendantOrSelfWord[_: P]: P[Unit] = parseWord("descendant-or-self")

  def divWord[_: P]: P[Unit] = parseWord("div")

  def documentNodeWord[_: P]: P[Unit] = parseWord("document-node")

  def elementWord[_: P]: P[Unit] = parseWord("element")

  def elseWord[_: P]: P[Unit] = parseWord("else")

  def emptySequenceWord[_: P]: P[Unit] = parseWord("empty-sequence")

  def eqWord[_: P]: P[Unit] = parseWord("eq")

  def everyWord[_: P]: P[Unit] = parseWord("every")

  def exceptWord[_: P]: P[Unit] = parseWord("except")

  def followingWord[_: P]: P[Unit] = parseWord("following")

  def followingSiblingWord[_: P]: P[Unit] = parseWord("following-sibling")

  def forWord[_: P]: P[Unit] = parseWord("for")

  def functionWord[_: P]: P[Unit] = parseWord("function")

  def geWord[_: P]: P[Unit] = parseWord("ge")

  def gtWord[_: P]: P[Unit] = parseWord("gt")

  def idivWord[_: P]: P[Unit] = parseWord("idiv")

  def ifWord[_: P]: P[Unit] = parseWord("if")

  def inWord[_: P]: P[Unit] = parseWord("in")

  def instanceWord[_: P]: P[Unit] = parseWord("instance")

  def intersectWord[_: P]: P[Unit] = parseWord("intersect")

  def isWord[_: P]: P[Unit] = parseWord("is")

  def itemWord[_: P]: P[Unit] = parseWord("item")

  def leWord[_: P]: P[Unit] = parseWord("le")

  def letWord[_: P]: P[Unit] = parseWord("let")

  def ltWord[_: P]: P[Unit] = parseWord("lt")

  def mapWord[_: P]: P[Unit] = parseWord("map")

  def modWord[_: P]: P[Unit] = parseWord("mod")

  def namespaceWord[_: P]: P[Unit] = parseWord("namespace")

  def namespaceNodeWord[_: P]: P[Unit] = parseWord("namespace-node")

  def neWord[_: P]: P[Unit] = parseWord("ne")

  def nodeWord[_: P]: P[Unit] = parseWord("node")

  def ofWord[_: P]: P[Unit] = parseWord("of")

  def orWord[_: P]: P[Unit] = parseWord("or")

  def parentWord[_: P]: P[Unit] = parseWord("parent")

  def precedingWord[_: P]: P[Unit] = parseWord("preceding")

  def precedingSiblingWord[_: P]: P[Unit] = parseWord("preceding-sibling")

  def processingInstructionWord[_: P]: P[Unit] = parseWord("processing-instruction")

  def returnWord[_: P]: P[Unit] = parseWord("return")

  def satisfiesWord[_: P]: P[Unit] = parseWord("satisfies")

  def schemaAttributeWord[_: P]: P[Unit] = parseWord("schema-attribute")

  def schemaElementWord[_: P]: P[Unit] = parseWord("schema-element")

  def selfWord[_: P]: P[Unit] = parseWord("self")

  def someWord[_: P]: P[Unit] = parseWord("some")

  def textWord[_: P]: P[Unit] = parseWord("text")

  def thenWord[_: P]: P[Unit] = parseWord("then")

  def toWord[_: P]: P[Unit] = parseWord("to")

  def treatWord[_: P]: P[Unit] = parseWord("treat")

  def unionWord[_: P]: P[Unit] = parseWord("union")

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

  private def symbolSeparator[_: P]: P[Unit] =
    P(End | CharPred(c => !NCName.canBePartOfNCName(c)))

  def parseWord[_: P](s: String): P[Unit] = {
    P(s ~ &(symbolSeparator))
  }
}
