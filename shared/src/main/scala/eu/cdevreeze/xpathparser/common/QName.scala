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

package eu.cdevreeze.xpathparser.common

/**
 * Lexical qualified name. See the QName type in the yaidom project.
 *
 * @author
 *   Chris de Vreeze
 */
enum QName(val prefixOption: Option[String], val localPart: String):
  case Unprefixed(override val localPart: String) extends QName(None, localPart)
  case Prefixed(prefix: String, override val localPart: String) extends QName(Some(prefix), localPart)

  override def toString: String = this match
    case Unprefixed(localPart)       => localPart
    case Prefixed(prefix, localPart) => s"$prefix:$localPart"

object QName:

  /** Creates a `QName` from an optional prefix and a localPart */
  def apply(prefixOption: Option[String], localPart: String): QName =
    prefixOption
      .map { pref =>
        Prefixed(pref, localPart)
      }
      .getOrElse(Unprefixed(localPart))

  /** Creates a `Prefixed` from a prefix and a localPart */
  def apply(prefix: String, localPart: String): QName = Prefixed(prefix, localPart)

  /**
   * Parses a `String` into a `QName`. The `String` (after trimming) must conform to the `toString` format of a
   * `Prefixed` or `Unprefixed`.
   */
  def parse(s: String): QName =
    val st = s.trim

    val arr = st.split(':')
    require(arr.size <= 2, s"Expected at most 1 colon in QName '${st}'")

    arr.size match
      case 1 => Unprefixed(st)
      case 2 => Prefixed(arr(0), arr(1))
      case _ => sys.error(s"Did not expect more than 1 colon in QName '${st}'")

  /**
   * Extractor turning a QName into a pair of an optional prefix, and a local part.
   *
   * With this extractor one can pattern match on arbitrary QNames, and not just on prefixed or unprefixed names.
   */
  def unapply(qname: QName): Option[(Option[String], String)] = qname match
    case Unprefixed(localPart)       => Some((None, localPart))
    case Prefixed(prefix, localPart) => Some((Some(prefix), localPart))
    case null                        => None
