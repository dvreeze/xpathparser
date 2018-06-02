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

package eu.cdevreeze.xpathparser

/**
 * Query API, as offered by the AST classes. It is inspired by the yaidom project.
 *
 * The query API methods may be somewhat verbose, but that is intentional. First of all, there are
 * multiple different "axes" that can be used in queries, such as child elements, descendant elements
 * or descendant-or-self elements (if we consider only forward axes). Second, when querying for
 * elements of specific types, there is something to be said for having normal parameters for those
 * types, instead of (more hidden) type parameters.
 *
 * @author Chris de Vreeze
 */
package object queryapi
