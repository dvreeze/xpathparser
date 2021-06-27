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

import eu.cdevreeze.xpathparser.ast.AnyWildcard
import eu.cdevreeze.xpathparser.ast.LocalNameWildcard
import eu.cdevreeze.xpathparser.ast.NamespaceWildcard
import eu.cdevreeze.xpathparser.ast.PrefixWildcard
import eu.cdevreeze.xpathparser.ast.Wildcard
import cats.parse.{Parser => P}

/**
 * Wildcard parsing support. No whitespace skipping is performed. See ws:explicit constraint.
 *
 * For re-usability without imposing any NoCut calls on using parsers, no cuts have been used.
 *
 * @author Chris de Vreeze
 */
object Wildcards {

  private val DT = DelimitingTerminals

  // Note the arbitrary order of parser value definitions below (due to deferred evaluation)

  val wildcard: P[Wildcard] =
    P.defer(P.oneOf(prefixWildcard :: localNameWildcard :: namespaceWildcard :: anyWildcard :: Nil))

  val prefixWildcard: P[PrefixWildcard] =
    P.defer(NCNames.ncName.soft <* DT.colonAsterisk).map { nm =>
      PrefixWildcard(nm)
    }

  val localNameWildcard: P[LocalNameWildcard] =
    P.defer(DT.asteriskColon.soft *> NCNames.ncName).map { nm =>
      LocalNameWildcard(nm)
    }

  val namespaceWildcard: P[NamespaceWildcard] =
    P.defer(DT.bracedUriLiteral.soft <* DT.asterisk).map { uriLit =>
      NamespaceWildcard(uriLit)
    }

  val anyWildcard: P[AnyWildcard.type] =
    P.defer(DT.asterisk).as(AnyWildcard)
}
