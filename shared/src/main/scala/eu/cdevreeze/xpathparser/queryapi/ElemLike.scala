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
 * Partial implementation of the ElemApi query API trait.
 *
 * @author Chris de Vreeze
 */
trait ElemLike[E <: ElemLike[E]] extends ElemApi[E] { self: E =>

  import ElemApi.anyElem

  def children: IndexedSeq[E]

  // Finding topmost descendant elements (of a certain type, obeying some predicate)

  final def findTopmostElems(p: E => Boolean): IndexedSeq[E] =
    children.flatMap(_.findTopmostElemsOrSelf(p))

  final def findAllTopmostElemsOfType[A <: E](cls: ClassTag[A]): IndexedSeq[A] =
    findTopmostElemsOfType(cls)(anyElem)

  final def findTopmostElemsOfType[A <: E](cls: ClassTag[A])(p: A => Boolean): IndexedSeq[A] =
    implicit val tag = cls

    findTopmostElems {
      case e: A if p(e) => true
      case e            => false
    } collect {
      case e: A => e
    }

  // Finding topmost descendant-or-self elements (of a certain type, obeying some predicate)

  final def findTopmostElemsOrSelf(p: E => Boolean): IndexedSeq[E] =
    if p(self) then
      IndexedSeq(self)
    else
      // Recursive calls
      children.flatMap(_.findTopmostElemsOrSelf(p))

  final def findAllTopmostElemsOrSelfOfType[A <: E](cls: ClassTag[A]): IndexedSeq[A] =
    findTopmostElemsOrSelfOfType(cls)(anyElem)

  final def findTopmostElemsOrSelfOfType[A <: E](cls: ClassTag[A])(p: A => Boolean): IndexedSeq[A] =
    implicit val tag = cls

    findTopmostElemsOrSelf {
      case e: A if p(e) => true
      case e            => false
    } collect {
      case e: A => e
    }

  // Filtering descendant elements (of a certain type, obeying some predicate)

  final def filterElems(p: E => Boolean): IndexedSeq[E] =
    children.flatMap(_.filterElemsOrSelf(p))

  final def findAllElems: IndexedSeq[E] =
    filterElems(_ => true)

  final def findAllElemsOfType[A <: E](cls: ClassTag[A]): IndexedSeq[A] =
    filterElemsOfType(cls)(anyElem)

  final def filterElemsOfType[A <: E](cls: ClassTag[A])(p: A => Boolean): IndexedSeq[A] =
    implicit val tag = cls

    filterElems {
      case e: A if p(e) => true
      case e            => false
    } collect {
      case e: A => e
    }

  // Filtering descendant-or-self elements (of a certain type, obeying some predicate)

  final def filterElemsOrSelf(p: E => Boolean): IndexedSeq[E] =
    // Recursive calls
    IndexedSeq(self).filter(p) ++ children.flatMap(_.filterElemsOrSelf(p))

  final def findAllElemsOrSelf: IndexedSeq[E] =
    filterElemsOrSelf(_ => true)

  final def findAllElemsOrSelfOfType[A <: E](cls: ClassTag[A]): IndexedSeq[A] =
    filterElemsOrSelfOfType(cls)(anyElem)

  final def filterElemsOrSelfOfType[A <: E](cls: ClassTag[A])(p: A => Boolean): IndexedSeq[A] =
    implicit val tag = cls

    filterElemsOrSelf {
      case e: A if p(e) => true
      case e            => false
    } collect {
      case e: A => e
    }

  // Finding an optional element (of a certain type, obeying some predicate)

  final def findElem(p: E => Boolean): Option[E] =
    // Not very efficient

    findTopmostElems(p).headOption

  final def findFirstElemOfType[A <: E](cls: ClassTag[A]): Option[A] =
    findElemOfType(cls)(anyElem)

  final def findElemOfType[A <: E](cls: ClassTag[A])(p: A => Boolean): Option[A] =
    implicit val tag = cls

    findElem {
      case e: A if p(e) => true
      case e            => false
    } collectFirst {
      case e: A => e
    }

  // Finding an optional element-or-self (of a certain type, obeying some predicate)

  final def findElemOrSelf(p: E => Boolean): Option[E] =
    // Not very efficient

    findTopmostElemsOrSelf(p).headOption

  final def findFirstElemOrSelfOfType[A <: E](cls: ClassTag[A]): Option[A] =
    findElemOrSelfOfType(cls)(anyElem)

  final def findElemOrSelfOfType[A <: E](cls: ClassTag[A])(p: A => Boolean): Option[A] =
    implicit val tag = cls

    findElemOrSelf {
      case e: A if p(e) => true
      case e            => false
    } collectFirst {
      case e: A => e
    }
}
