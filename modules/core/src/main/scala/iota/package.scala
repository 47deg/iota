/*
 * Copyright 2016-2017 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package object iota extends Scala211Compat {

  /** The terminal element of a type constructor list */
  type KNil <: KList

  /** A type constructor list characterized by a head type
    * constructor and a list of tail type constructors
    *
    * @tparam H the head type constructor
    * @tparam T the list of tail type constructors
    */
  type KCons[H[_], T <: KList] <: KList

  /** The terminal element of a type list */
  type TNil <: TList

  /** A type list characterized by a head type and a list of tail types
    *
    * @tparam H the head type
    * @tparam T the list of tail types
    */
  type TCons[H, T <: TList] <: TList

}
