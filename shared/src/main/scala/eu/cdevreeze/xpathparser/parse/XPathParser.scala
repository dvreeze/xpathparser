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

import eu.cdevreeze.xpathparser.ast.XPathExpr
import cats.parse.{Parser => P}

/**
 * XPath 3.1 parsing support, using cats-parse.
 *
 * Usage:
 * {{{
 * XPathParser.xpathExpr.parse(xpathString)
 * }}}
 *
 * @author Chris de Vreeze
 */
object XPathParser:

  import Whitespace._

  /**
   * Parser for an XPath expression. Usage: `xpathExpr.parse(xpathString)`. Comments are not supported,
   * so will lead to parsing failures.
   *
   * The parser consumes the entire input string or else parsing cannot be successful. Leading or trailing
   * whitespace is silently ignored.
   */
  val xpathExpr: P[XPathExpr] = P.defer {
    (P.start.soft.with1 *> XPathElemParser.expr).soft <* (whitespaces0.soft ~ P.end)
  }
