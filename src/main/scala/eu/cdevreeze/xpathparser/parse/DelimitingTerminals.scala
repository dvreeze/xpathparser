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
  import fastparse.all._

  // Exclamation marks and not-equals symbol

  val exclamationMark: P[Unit] = P("!" ~ !"=")

  val notEquals: P[Unit] = P("!=")

  // String literals

  val stringLiteral: P[StringLiteral] = StringLiterals.stringLiteral

  val hash: P[Unit] = P("#")

  val dollar: P[Unit] = P("$")

  val openParenthesis: P[Unit] = P("(")

  val closeParenthesis: P[Unit] = P(")")

  val asterisk: P[Unit] = P("*")

  val plus: P[Unit] = P("+")

  val comma: P[Unit] = P(",")

  val minus: P[Unit] = P("-")

  // Single and double dots

  val dot: P[Unit] = P("." ~ !".")

  val doubleDot: P[Unit] = P("..")

  // Single and double slashes

  val slash: P[Unit] = P("/" ~ !"/")

  val doubleSlash: P[Unit] = P("//")

  // Single and double colons and assignment symbol

  val colon: P[Unit] = P(":" ~ !(":" | "="))

  val doubleColon: P[Unit] = P("::")

  val assignmentSymbol: P[Unit] = P(":=")

  // Symbols starting with less-than character

  val lessThan: P[Unit] = P("<" ~ !("=" | "<"))

  val lessThanOrEqual: P[Unit] = P("<=")

  val precedes: P[Unit] = P("<<")

  // Symbols starting with greater-than character

  val greaterThan: P[Unit] = P(">" ~ !("=" | ">"))

  val greaterThanOrEqual: P[Unit] = P(">=")

  val follows: P[Unit] = P(">>")

  val equals: P[Unit] = P("=")

  val questionMark: P[Unit] = P("?")

  val at: P[Unit] = P("@")

  // Braced URI literal

  val bracedUriLiteral: P[BracedUriLiteral] =
    P(("Q{" ~ CharsWhile(c => (c != '{') && (c != '}')) ~ "}").!) filter (v => BracedUriLiteral.canBeBracedUriLiteral(v)) map { v =>
      BracedUriLiteral.parse(v)
    }

  val openBracket: P[Unit] = P("[")

  val closeBracket: P[Unit] = P("]")

  val openBrace: P[Unit] = P("{")

  val closeBrace: P[Unit] = P("}")

  // String concatenation and union symbols

  val verticalBar: P[Unit] = P("|" ~ !"|")

  val doubleVerticalBar: P[Unit] = P("||")

  object StringLiterals {

    val stringLiteral: P[StringLiteral] =
      P(aposStringLiteral | quoteStringLiteral)

    // TODO Make more efficient

    // Note the use of cuts here, which may hinder re-use in XPathParser, unless we switch off cuts where needed.

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
}
