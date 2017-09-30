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
 * NCName, that is, a non-colon name.
 *
 * @author Chris de Vreeze
 */
final case class NCName(name: String) {
  require(!name.contains(':'), s"Not an NCName: '$name'")
}

object NCName {

  /**
   * Returns true if the given string can start valid non-colon names. This is the same as
   * saying that the string is a valid non-colon name, so `canBeNCName(s)` is returned.
   */
  def canBeStartOfNCName(s: String): Boolean = {
    canBeNCName(s)
  }

  /**
   * Returns true if the given string is a valid non-colon name.
   *
   * Disclaimer: Names starting with "xml" are not excluded, and names containing non-BMP characters are
   * not included.
   */
  def canBeNCName(s: String): Boolean = {
    s.nonEmpty && canBeStartOfNCName(s.charAt(0)) && s.drop(1).forall(c => canBePartOfNCName(c))
  }

  /**
   * Returns the same as `Names.canBeStartOfName(c)`, but excluding the colon.
   */
  def canBeStartOfNCName(c: Char): Boolean = {
    // By disallowing digits and dots as first characters of an NCName, an XPath parser does not confuse
    // NCNames with numeric literals, for example.

    (c != ':') && Names.canBeStartOfName(c)
  }

  /**
   * Returns the same as `Names.canBePartOfName(c)`, but excluding the colon.
   */
  def canBePartOfNCName(c: Char): Boolean = {
    (c != ':') && Names.canBePartOfName(c)
  }
}
