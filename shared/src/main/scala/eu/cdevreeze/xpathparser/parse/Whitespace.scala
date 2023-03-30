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

import cats.parse.{Parser => P}
import cats.parse.Parser0

/**
 * Support for ignorable whitespace during parsing.
 *
 * @author
 *   Chris de Vreeze
 */
object Whitespace:

  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val whitespaces0: Parser0[Unit] = whitespace.rep0.void
  val whitespaces: P[Unit] = whitespace.rep.void

  implicit class SkippingWS[A](val parser: P[A]) extends AnyVal:

    def skipWS: P[A] = Whitespace.skippingWS(parser)

  def skippingWS[A](parser: P[A]): P[A] = P.defer(whitespaces0.soft.with1 *> parser)

  def skippingWS[A](parser0: Parser0[A]): Parser0[A] = P.defer0(whitespaces0.soft *> parser0)
