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

package eu.cdevreeze.xpathparser.queryapi

import scala.reflect.ClassTag

/**
 * Common purely abstract query API trait for querying abstract syntax trees (or other object trees).
 *
 * It has been heavily inspired by the yaidom project for XML querying.
 *
 * @author Chris de Vreeze
 */
trait ElemApi[E <: ElemApi[E]]:

  /**
   * Finds all child elements.
   */
  def children: IndexedSeq[E]

  // Finding topmost descendant elements (of a certain type, obeying some predicate)

  /**
   * Finds all topmost descendant elements obeying the given predicate.
   */
  def findTopmostElems(p: E => Boolean): IndexedSeq[E]

  /**
   * Finds all topmost descendant elements of the given element type.
   */
  def findAllTopmostElemsOfType[A <: E](cls: ClassTag[A]): IndexedSeq[A]

  /**
   * Finds all topmost descendant elements of the given element type obeying the given predicate.
   */
  def findTopmostElemsOfType[A <: E](cls: ClassTag[A])(p: A => Boolean): IndexedSeq[A]

  // Finding topmost descendant-or-self elements (of a certain type, obeying some predicate)

  /**
   * Finds all topmost elements-or-self obeying the given predicate. This is a core method in that
   * several methods are implemented directly or indirectly in terms of this one.
   */
  def findTopmostElemsOrSelf(p: E => Boolean): IndexedSeq[E]

  /**
   * Finds all topmost descendant-or-self elements of the given element type.
   */
  def findAllTopmostElemsOrSelfOfType[A <: E](cls: ClassTag[A]): IndexedSeq[A]

  /**
   * Finds all topmost descendant-or-self elements of the given element type obeying the given predicate.
   */
  def findTopmostElemsOrSelfOfType[A <: E](cls: ClassTag[A])(p: A => Boolean): IndexedSeq[A]

  // Filtering descendant elements (of a certain type, obeying some predicate)

  /**
   * Finds all descendant elements obeying the given predicate.
   */
  def filterElems(p: E => Boolean): IndexedSeq[E]

  /**
   * Finds all descendant elements.
   */
  def findAllElems: IndexedSeq[E]

  /**
   * Finds all descendant elements of the given element type.
   */
  def findAllElemsOfType[A <: E](cls: ClassTag[A]): IndexedSeq[A]

  /**
   * Finds all descendant elements of the given element type obeying the given predicate.
   */
  def filterElemsOfType[A <: E](cls: ClassTag[A])(p: A => Boolean): IndexedSeq[A]

  // Filtering descendant-or-self elements (of a certain type, obeying some predicate)

  /**
   * Finds all descendant-or-self elements obeying the given predicate. This is a core method in that
   * several methods are implemented directly or indirectly in terms of this one.
   */
  def filterElemsOrSelf(p: E => Boolean): IndexedSeq[E]

  /**
   * Finds all descendant-or-self elements.
   */
  def findAllElemsOrSelf: IndexedSeq[E]

  /**
   * Finds all descendant-or-self elements of the given element type.
   */
  def findAllElemsOrSelfOfType[A <: E](cls: ClassTag[A]): IndexedSeq[A]

  /**
   * Finds all descendant-or-self elements of the given element type obeying the given predicate.
   */
  def filterElemsOrSelfOfType[A <: E](cls: ClassTag[A])(p: A => Boolean): IndexedSeq[A]

  // Finding an optional element (of a certain type, obeying some predicate)

  /**
   * Finds the first descendant element obeying the given predicate, if any, returning an optional result.
   */
  def findElem(p: E => Boolean): Option[E]

  /**
   * Finds the first descendant element of the given element type, if any, returning an optional result.
   */
  def findFirstElemOfType[A <: E](cls: ClassTag[A]): Option[A]

  /**
   * Finds the first descendant element of the given element type obeying the given predicate, if any,
   * returning an optional result.
   */
  def findElemOfType[A <: E](cls: ClassTag[A])(p: A => Boolean): Option[A]

  // Finding an optional element-or-self (of a certain type, obeying some predicate)

  /**
   * Finds the first descendant-or-self element obeying the given predicate, if any, returning an optional result.
   */
  def findElemOrSelf(p: E => Boolean): Option[E]

  /**
   * Finds the first descendant-or-self element of the given element type, if any, returning an optional result.
   */
  def findFirstElemOrSelfOfType[A <: E](cls: ClassTag[A]): Option[A]

  /**
   * Finds the first descendant-or-self element of the given element type obeying the given predicate, if any,
   * returning an optional result.
   */
  def findElemOrSelfOfType[A <: E](cls: ClassTag[A])(p: A => Boolean): Option[A]

object ElemApi:

  /**
   * Element predicate that returns true for each element.
   */
  val anyElem: ElemApi[_] => Boolean = { _ => true }
