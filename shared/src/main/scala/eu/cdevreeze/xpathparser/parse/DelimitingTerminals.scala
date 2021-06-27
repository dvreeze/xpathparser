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
import eu.cdevreeze.xpathparser.ast.StringLiteral
import cats.parse.Accumulator0._
import cats.parse.{Parser => P}

import scala.util.chaining._

/**
 * Delimiting terminal symbols. No whitespace is skipped during this tokenization.
 * Lookahead is applied when needed to distinguish between different terminal symbols starting with
 * the same character.
 *
 * When using this object, make sure that a "-" symbol requires a symbol separator (whitespace or comments)
 * if it follows a QName or NCName. The same holds for a "." symbol. Also, if a "." follows or precedes a
 * numeric literal, it requires a symbol separator. (Other than that, symbol separators are only needed
 * for non-delimiting terminal symbols.)
 *
 * @author Chris de Vreeze
 */
object DelimitingTerminals {

  // Exclamation marks and not-equals symbol

  val exclamationMark: P[Unit] = (P.string("!").soft ~ P.string("=").unary_!).void

  val notEquals: P[Unit] = P.string("!=")

  // String literals

  val stringLiteral: P[StringLiteral] = StringLiterals.stringLiteral

  val hash: P[Unit] = P.string("#")

  val dollar: P[Unit] = P.string("$")

  val openParenthesis: P[Unit] = P.string("(")

  val closeParenthesis: P[Unit] = P.string(")")

  // Asterisk and asterisk-colon

  val asterisk: P[Unit] = (P.string("*").soft ~ P.string(":").unary_!).void

  val asteriskColon: P[Unit] = P.string("*:")

  val plus: P[Unit] = P.string("+")

  val comma: P[Unit] = P.string(",")

  val minus: P[Unit] = P.string("-")

  // Single and double dots

  val dot: P[Unit] = (P.string(".").soft ~ P.string(".").unary_!).void

  val doubleDot: P[Unit] = P.string("..")

  // Single and double slashes

  val slash: P[Unit] = (P.string("/").soft ~ P.string("/").unary_!).void

  val doubleSlash: P[Unit] = P.string("//")

  // Single and double colons, colon-asterisk and assignment symbol

  val colon: P[Unit] = (P.string(":").soft ~ (P.string(":") | P.string("*") | P.string("=")).unary_!).void

  val doubleColon: P[Unit] = P.string("::")

  val colonAsterisk: P[Unit] = P.string(":*")

  val assignmentSymbol: P[Unit] = P.string(":=")

  // Symbols starting with less-than character

  val lessThan: P[Unit] = (P.string("<").soft ~ (P.string("=") | P.string("<")).unary_!).void

  val lessThanOrEqual: P[Unit] = P.string("<=")

  val precedes: P[Unit] = P.string("<<")

  // Symbols starting with greater-than character

  val greaterThan: P[Unit] = (P.string(">").soft ~ (P.string("=") | P.string(">")).unary_!).void

  val greaterThanOrEqual: P[Unit] = P.string(">=")

  val follows: P[Unit] = P.string(">>")

  // Symbols starting with the equals character

  val equals: P[Unit] = (P.string("=").soft ~ P.string(">").unary_!).void

  val doubleArrow: P[Unit] = P.string("=>")

  val questionMark: P[Unit] = P.string("?")

  val at: P[Unit] = P.string("@")

  // Braced URI literal

  val bracedUriLiteral: P[BracedUriLiteral] = P.defer {
    ((P.string("Q{").soft *> P.charsWhile0(isAllowedNamespaceUriChar)).soft <* P.string("}")).map { rawNs =>
      if (rawNs.isEmpty) BracedUriLiteral(None) else BracedUriLiteral(Some(rawNs))
    }
  }

  val openBracket: P[Unit] = P.string("[")

  val closeBracket: P[Unit] = P.string("]")

  val openBrace: P[Unit] = P.string("{")

  val closeBrace: P[Unit] = P.string("}")

  // String concatenation and union symbols

  val verticalBar: P[Unit] = (P.string("|").soft ~ P.string("|").unary_!).void

  val doubleVerticalBar: P[Unit] = P.string("||")

  private def isAllowedNamespaceUriChar(c: Char): Boolean = {
    // TODO Is this correct?

    (c != '{') && (c != '}') && !java.lang.Character.isWhitespace(c)
  }

  object StringLiterals {

    private def isApos(c: Char): Boolean = c == '\''
    private def isNotApos(c: Char): Boolean = !isApos(c)

    private def isQuote(c: Char): Boolean = c == '"'
    private def isNotQuote(c: Char): Boolean = !isQuote(c)

    private val apos: P[String] = P.charWhere(isApos).string
    private val quote: P[String] = P.charWhere(isQuote).string

    // Note the heavy use of backtracking here, if escaped aps/quote is found. I did not get a better alternative without backtracking working.

    private def aposStringLiteralPart(oddAposCountAtEnd: Boolean): P[String] = {
      val p: Int => Boolean = { n =>
        if (oddAposCountAtEnd) (n % 2 != 0) else (n % 2 == 0)
      }

      (P.charsWhile0(isNotApos).soft.with1 ~ (P.charsWhile(isApos).filter(s => p(s.size)).backtrack)).string
    }

    private val aposStringLiteralContent: P[String] = P.defer {
      (aposStringLiteralPart(false).rep0.soft.with1 ~ aposStringLiteralPart(true)).map {
        case (initParts: Seq[String], lastPart: String) =>
          initParts.mkString.concat(lastPart.ensuring(_.endsWith("'")).init).pipe { v =>
            v.replace("''", "'")
          }
      }
    }

    private val aposStringLiteral: P[StringLiteral] = P.defer {
      (apos.soft *> aposStringLiteralContent).map { v =>
        StringLiteral(v)
      }
    }

    private def quoteStringLiteralPart(oddQuoteCountAtEnd: Boolean): P[String] = {
      val p: Int => Boolean = { n =>
        if (oddQuoteCountAtEnd) (n % 2 != 0) else (n % 2 == 0)
      }

      (P.charsWhile0(isNotQuote).soft.with1 ~ (P.charsWhile(isQuote).filter(s => p(s.size)).backtrack)).string
    }

    private val quoteStringLiteralContent: P[String] = P.defer {
      (quoteStringLiteralPart(false).rep0.soft.with1 ~ quoteStringLiteralPart(true)).map {
        case (initParts: Seq[String], lastPart: String) =>
          initParts.mkString.concat(lastPart.ensuring(_.endsWith("\"")).init).pipe { v =>
            v.replace("\"\"", "\"")
          }
      }
    }

    private val quoteStringLiteral: P[StringLiteral] = P.defer {
      (quote.soft *> quoteStringLiteralContent).map { v =>
        StringLiteral(v)
      }
    }

    val stringLiteral: P[StringLiteral] =
      P.defer(aposStringLiteral | quoteStringLiteral)
  }
}
