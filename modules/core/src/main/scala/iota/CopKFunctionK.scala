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

import cats._

abstract class CopKFunctionK[F[_], G[_]] extends (F ~> G)

/** Methods to create [[FunctionK]] instances for [[CopK]] coproducts */
object CopKFunctionK {

  /** Creates a [[FunctionK]] from `F` to `G` by fanning in respective
    * FunctionKs for type all type constructors in the coproduct `F`.
    *
    * The respective FunctionKs are pulled from the input `args` on
    * an as-needed basis; superfluous arguments are ignored.
    */
  def of[F[a] <: CopK[_, a], G[_]](args: Any*): F ~> G =
    macro internal.CopKFunctionKMacros.of[F, G]

  /** Creates a [[FunctionK]] from `F` to `G` by fanning in respective
    * FunctionKs for type all type constructors in the coproduct `F`.
    *
    * The respective FunctionKs are summoned implicitly on an an
    * as-needed basis.
    */
  def summon[F[a] <: CopK[_, a], G[_]]: F ~> G =
    macro internal.CopKFunctionKMacros.summon[F, G]
}
