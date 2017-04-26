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

package iota

/** A heterogenous list of type constructors */
trait KList

object KList {

  /** A syntactic sugar alias for [[KCons]] */
  type ::[H[_], T <: KList] = KCons[H, T]

  /** A syntactic sugar alias for [[KCons]] */
  type :::[H[_], T <: KList] = KCons[H, T]

  /** A type class that witnesses the position of type constructor `F` in type
    * constructor list `L`
    */
  trait Pos[L <: KList, F[_]] {
    def index: Int
  }

  object Pos {
    def apply[L <: KList, F[_]](implicit ev: Pos[L, F]): Pos[L, F] = ev
    implicit def materializePos[L <: KList, F[_]]: Pos[L, F] =
      macro internal.KListMacros.materializePos[L, F]
  }
}
