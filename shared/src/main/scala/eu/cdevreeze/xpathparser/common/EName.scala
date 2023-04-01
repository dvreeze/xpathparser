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
 * Expanded name, containing a local part and optional namespace name. See the EName type in the yaidom project.
 *
 * @author
 *   Chris de Vreeze
 */
enum EName(val namespaceUriOption: Option[String], val localPart: String):
  case WithoutNamespace(override val localPart: String) extends EName(None, localPart)
  case WithNamespace(namespace: String, override val localPart: String) extends EName(Some(namespace), localPart)

  /** Given an optional prefix, creates a `QName` from this `EName` */
  def toQName(prefixOption: Option[String]): QName =
    require(
      namespaceUriOption.isDefined || prefixOption.isEmpty,
      s"Prefix only allowed if namespace non-empty in EName '${this}'"
    )
    QName(prefixOption, localPart)

  /** The `String` representation, in the format of the `javax.xml.namespace.QName.toString` method */
  override def toString: String = namespaceUriOption match
    case None        => localPart
    case Some(nsUri) => "{" + nsUri + "}" + localPart

object EName:

  def apply(namespaceUriOption: Option[String], localPart: String): EName = namespaceUriOption match
    case None     => EName.WithoutNamespace(localPart)
    case Some(ns) => EName.WithNamespace(ns, localPart)

  /** Creates an `EName` from a namespaceUri and a localPart */
  def apply(namespaceUri: String, localPart: String): EName = EName.WithNamespace(namespaceUri, localPart)

  /**
   * Parses a `String` into an `EName`. The `String` (after trimming) must conform to the `toString` format of an
   * `EName`.
   */
  def parse(s: String): EName =
    val st = s.trim

    if st.startsWith("{") then
      val idx = st.indexOf('}')
      require(idx >= 2 && idx < st.length - 1, s"Opening brace not closed or at incorrect location in EName '${st}'")
      val ns = st.substring(1, idx)
      val localPart = st.substring(idx + 1)
      EName.WithNamespace(ns, localPart)
    else
      require(st.indexOf("{") < 0, s"No opening brace allowed unless at the beginning in EName '${st}'")
      require(st.indexOf("}") < 0, s"Closing brace without matching opening brace not allowed in EName '${st}'")
      EName.WithoutNamespace(st)
