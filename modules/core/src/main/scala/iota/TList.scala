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

/** A heterogenous list of types */
trait TList

object TList {

  /** A syntactic sugar alias for [[TCons]] */
  type ::[H, T <: TList] = TCons[H, T]

  /** A syntactic sugar alias for [[TCons]] */
  type :::[H, T <: TList] = TCons[H, T]

  /** A type class that witnesses the position of type `A` in type
    * list `L`
    */
  trait Pos[L <: TList, A] {
    def index: Int
  }

  object Pos {
    def apply[L <: TList, A](implicit ev: Pos[L, A]): Pos[L, A] = ev
    implicit def materializePos[L <: TList, A]: Pos[L, A] =
      macro internal.TypeListMacros.materializeTListPos[L, A]
  }

  object Op {
    type Concat [L <: TList, R <: TList]        <: TList
    type Reverse[L <: TList]                    <: TList
    type Take   [N <: SingletonInt, L <: TList] <: TList
    type Drop   [N <: SingletonInt, L <: TList] <: TList
    type Remove [T, L <: TList]                 <: TList
  }

  trait Compute[L <: TList] {
    type Out <: TList
  }

  object Compute {
    type Aux[L <: TList, O <: TList] = Compute[L] { type Out = O }

    def apply[L <: TList](implicit ev: Compute[L]): Compute.Aux[L, ev.Out] = ev
    implicit def materializeCompute[L <: TList, O <: TList]: Aux[L, O] =
      macro internal.TypeListMacros.materializeTListCompute[L, O]
  }

  trait Gen[A] {
    type Repr <: TList
  }

  object Gen {
    type Aux[A, R <: TList] = Gen[A] { type Repr = R }

    def apply[A](implicit ev: Gen[A]): Gen.Aux[A, ev.Repr] = ev
    implicit def materializeGen[A, R <: TList]: Aux[A, R] =
      macro internal.TypeListMacros.materializeTListGen[A, R]
  }


}
