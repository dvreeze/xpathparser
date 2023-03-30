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

package eu.cdevreeze.xpathparser.ast

/**
 * Names, as per the XML specification. See for example https://www.w3.org/TR/REC-xml/#NT-Name.
 *
 * Only characters in the Unicode BMP (max. \uFFFF) are considered. So the range from #x10000 to #xEFFFF is not
 * recognized as valid name characters in this implementation.
 *
 * The functions of this class must be fast, because they are typically called very many times during parsing of XPath
 * expressions.
 *
 * Classes like NCName lean on this object. An NCName is an XML name without colon.
 *
 * @author
 *   Chris de Vreeze
 */
object Names:

  // TODO What about names like "xml"?

  // See for example https://en.wikibooks.org/wiki/Unicode/Character_reference/0000-0FFF

  /**
   * Returns true if the given string can start valid XML names. This is the same as saying that the string is a valid
   * XML name, so `canBeName(s)` is returned.
   */
  def canBeStartOfName(s: String): Boolean =
    canBeName(s)

  /**
   * Returns true if the given string is a valid XML name. Names starting with "xml" are not excluded, and names
   * containing non-BMP characters are not included.
   */
  def canBeName(s: String): Boolean =
    s.nonEmpty && canBeStartOfName(s.charAt(0)) && s.drop(1).forall(c => canBePartOfName(c))

  /**
   * Returns true if the character can be a start of an XML name. Only 2-byte characters in the BMP are considered.
   *
   * It returns the same as method canBePartOfName, except that some characters are excluded, such as digits, dash, dot
   * and combining diacritical marks.
   */
  def canBeStartOfName(c: Char): Boolean =
    charEquals(c, ':') ||
      charIsInRange(c, 'A', 'Z') ||
      charEquals(c, '_') ||
      charIsInRange(c, 'a', 'z') ||
      charIsInRange(c, '\u00C0', '\u00D6') ||
      charIsInRange(c, '\u00D8', '\u00F6') ||
      charIsInRange(c, '\u00F8', '\u02FF') ||
      charIsInRange(c, '\u0370', '\u037D') ||
      charIsInRange(c, '\u037F', '\u1FFF') ||
      charIsInRange(c, '\u200C', '\u200D') ||
      charIsInRange(c, '\u2070', '\u218F') ||
      charIsInRange(c, '\u2C00', '\u2FEF') ||
      charIsInRange(c, '\u3001', '\uD7FF') ||
      charIsInRange(c, '\uF900', '\uFDCF') ||
      charIsInRange(c, '\uFDF0', '\uFFFD')

  /**
   * Returns true if the character can be a part of an XML name. Only 2-byte characters in the BMP are considered.
   *
   * Some excluded characters are: multiplication sign ('\u00D7'), (other) plus sign ('\u00F7'), at-symbol, dollar sign,
   * percentage sign, ampersand, slash, plus sign ('\u002B'), comma, semicolon, open parenthesis, close parenthesis,
   * open bracket, close bracket, open brace, close brace, smaller-than symbol, greater-than symbol, equals sign, single
   * quote ('\u0060'), double quote ('\u0022'), asterisk, exclamation mark, question mark, and '#'.
   *
   * Note that these excluded characters are important to distinguish names from XPath language elements such as
   * operators and (string and numeric) literals.
   */
  def canBePartOfName(c: Char): Boolean =
    canBeStartOfName(c) ||
      charEquals(c, '-') ||
      charEquals(c, '.') ||
      charIsInRange(c, '0', '9') ||
      charEquals(c, '\u00B7') ||
      charIsInRange(c, '\u0300', '\u036F') ||
      charIsInRange(c, '\u203F', '\u2040')

  private def charEquals(c: Char, otherChar: Char): Boolean =
    c == otherChar

  private def charIsInRange(c: Char, low: Char, high: Char): Boolean =
    (c >= low) && (c <= high)
