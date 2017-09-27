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

package iota  //#=cats
package iotaz //#=scalaz

/** A heterogenous list of type constructors */
trait TListK

object TListK {

  /** A syntactic sugar alias for [[TConsK]] */
  type ::[H[_], T <: TListK] = TConsK[H, T]

  /** A syntactic sugar alias for [[TConsK]] */
  type :::[H[_], T <: TListK] = TConsK[H, T]

  /** A type class that witnesses the position of type constructor `F` in type
    * constructor list `L`
    */
  trait Pos[L <: TListK, F[_]] {
    def index: Int
  }

  object Pos {
    def apply[L <: TListK, F[_]](implicit ev: Pos[L, F]): Pos[L, F] = ev
    implicit def materializePos[L <: TListK, F[_]]: Pos[L, F] =
      macro internal.TypeListMacros.materializeTListKPos[L, F]
  }

  object Op {
    type Concat [L <: TListK, R <: TListK]       <: TListK
    type Reverse[L <: TListK]                    <: TListK
    type Take   [N <: SingletonInt, L <: TListK] <: TListK
    type Drop   [N <: SingletonInt, L <: TListK] <: TListK
    type Remove [K[_], L <: TListK]              <: TListK
  }

  trait Compute[L <: TListK] {
    type Out <: TListK
  }

  object Compute {
    type Aux[L <: TListK, O <: TListK] = Compute[L] { type Out = O }

    def apply[L <: TListK](implicit ev: Compute[L]): Compute.Aux[L, ev.Out] = ev
    implicit def materializeCompute[L <: TListK, O <: TListK]: Aux[L, O] =
      macro internal.TypeListMacros.materializeTListKCompute[L, O]
  }

}
