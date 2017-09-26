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

import eu.cdevreeze.xpathparser.common

/**
 * EQName, so either a URIQualifiedName or a QName.
 *
 * @author Chris de Vreeze
 */
sealed trait EQName

object EQName {

  final case class QName(qname: common.QName) extends EQName {

    override def toString: String = qname.toString
  }

  final case class URIQualifiedName(ename: common.EName) extends EQName {

    override def toString: String = ename match {
      case common.EName(None, localPart)     => s"Q{}$localPart"
      case common.EName(Some(ns), localPart) => s"Q{$ns}$localPart"
    }
  }

  object QName {

    def apply(s: String): QName = {
      parse(s)
    }

    def parse(s: String): QName = {
      QName(common.QName.parse(s))
    }
  }

  object URIQualifiedName {

    def parse(s: String): URIQualifiedName = {
      require(s.startsWith("Q{"), s"String '$s' is not a URIQualifiedName, because it does not start with 'Q{'")
      require(s.contains("}"), s"String '$s' is not a URIQualifiedName, because it does not contain '}'")
      require(!s.endsWith("}"), s"String '$s' is not a URIQualifiedName, because it ends with '}'")

      if (s.startsWith("Q{}")) {
        URIQualifiedName(common.EName.parse(s.drop(3)))
      } else {
        // Dropping the character "Q", we have James Clark notation to parse
        URIQualifiedName(common.EName.parse(s.drop(1)))
      }
    }
  }

  def parse(s: String): EQName = {
    if (s.startsWith("Q{")) URIQualifiedName.parse(s) else QName.parse(s)
  }
}
