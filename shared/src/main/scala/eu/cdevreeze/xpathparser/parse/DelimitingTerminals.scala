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
import fastparse.NoWhitespace._

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
  import fastparse._

  // Exclamation marks and not-equals symbol

  def exclamationMark[_: P]: P[Unit] = P("!" ~ !"=")

  def notEquals[_: P]: P[Unit] = P("!=")

  // String literals

  def stringLiteral[_: P]: P[StringLiteral] = StringLiterals.stringLiteral

  def hash[_: P]: P[Unit] = P("#")

  def dollar[_: P]: P[Unit] = P("$")

  def openParenthesis[_: P]: P[Unit] = P("(")

  def closeParenthesis[_: P]: P[Unit] = P(")")

  // Asterisk and asterisk-colon

  def asterisk[_: P]: P[Unit] = P("*" ~ !":")

  def asteriskColon[_: P]: P[Unit] = P("*:")

  def plus[_: P]: P[Unit] = P("+")

  def comma[_: P]: P[Unit] = P(",")

  def minus[_: P]: P[Unit] = P("-")

  // Single and double dots

  def dot[_: P]: P[Unit] = P("." ~ !".")

  def doubleDot[_: P]: P[Unit] = P("..")

  // Single and double slashes

  def slash[_: P]: P[Unit] = P("/" ~ !"/")

  def doubleSlash[_: P]: P[Unit] = P("//")

  // Single and double colons, colon-asterisk and assignment symbol

  def colon[_: P]: P[Unit] = P(":" ~ !(":" | "*" | "="))

  def doubleColon[_: P]: P[Unit] = P("::")

  def colonAsterisk[_: P]: P[Unit] = P(":*")

  def assignmentSymbol[_: P]: P[Unit] = P(":=")

  // Symbols starting with less-than character

  def lessThan[_: P]: P[Unit] = P("<" ~ !("=" | "<"))

  def lessThanOrEqual[_: P]: P[Unit] = P("<=")

  def precedes[_: P]: P[Unit] = P("<<")

  // Symbols starting with greater-than character

  def greaterThan[_: P]: P[Unit] = P(">" ~ !("=" | ">"))

  def greaterThanOrEqual[_: P]: P[Unit] = P(">=")

  def follows[_: P]: P[Unit] = P(">>")

  // Symbols starting with the equals character

  def equals[_: P]: P[Unit] = P("=" ~ !">")

  def doubleArrow[_: P]: P[Unit] = P("=>")

  def questionMark[_: P]: P[Unit] = P("?")

  def at[_: P]: P[Unit] = P("@")

  // Braced URI literal

  def bracedUriLiteral[_: P]: P[BracedUriLiteral] =
    P("Q{" ~ CharPred(isAllowedNamespaceUriChar).rep.! ~ "}") map {
      rawNs => if (rawNs.isEmpty) BracedUriLiteral(None) else BracedUriLiteral(Some(rawNs))
    }

  def openBracket[_: P]: P[Unit] = P("[")

  def closeBracket[_: P]: P[Unit] = P("]")

  def openBrace[_: P]: P[Unit] = P("{")

  def closeBrace[_: P]: P[Unit] = P("}")

  // String concatenation and union symbols

  def verticalBar[_: P]: P[Unit] = P("|" ~ !"|")

  def doubleVerticalBar[_: P]: P[Unit] = P("||")

  private def isAllowedNamespaceUriChar(c: Char): Boolean = {
    // TODO Is this correct?

    (c != '{') && (c != '}') && !java.lang.Character.isWhitespace(c)
  }

  object StringLiterals {

    def stringLiteral[_: P]: P[StringLiteral] =
      P(aposStringLiteral | quoteStringLiteral)

    // TODO Make more efficient

    // Note the use of cuts here, which may hinder re-use in XPathParser, unless we switch off cuts where needed.

    private def aposStringLiteral[_: P]: P[StringLiteral] =
      P("'" ~/ (escapeApos | nonEscapedCharInAposStringLiteral).rep.! ~ "'") map { v =>
        // Why do we still need the "unescaping" here?

        StringLiteral(v.replace("''", "'"))
      }

    private def quoteStringLiteral[_: P]: P[StringLiteral] =
      P("\"" ~/ (escapeQuote | nonEscapedCharInQuoteStringLiteral).rep.! ~ "\"") map { v =>
        // Why do we still need the "unescaping" here?

        StringLiteral(v.replace("\"\"", "\""))
      }

    private def escapeApos[_: P]: P[String] =
      P("'".rep(exactly = 2).!) map (_.substring(0, 1).ensuring(_.length == 1))

    private def nonEscapedCharInAposStringLiteral[_: P]: P[String] =
      P(CharPred(_ != '\'').!) map (_.ensuring(_.length == 1))

    private def escapeQuote[_: P]: P[String] =
      P("\"".rep(exactly = 2).!) map (_.substring(0, 1).ensuring(_.length == 1))

    private def nonEscapedCharInQuoteStringLiteral[_: P]: P[String] =
      P(CharPred(_ != '"').!) map (_.ensuring(_.length == 1))
  }
}
