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

package iotaexamples

import iota._

import scala.Predef._

object NestedCoproducts extends App {

  case class Op0[A]()
  case class Op1[A]()
  case class Op2[A]()
  case class Op3[A]()
  case class Op4[A]()
  case class Op5[A]()

  import TListK.::
  import TListK.Op.Concat
  import TListK.Op.Reverse

  type AlgA[A] = CopK[Op0 :: Op1 :: Op2 :: TNilK, A]
  type AlgB[A] = CopK[Op3 :: Op4 :: Op5 :: TNilK, A]

  type AFwd[A] = CopK[Concat[AlgA[_]#L, AlgB[_]#L], A]
  type ARev[A] = CopK[Reverse[Concat[AlgA[_]#L, AlgB[_]#L]], A]

  implicit class InjectAny[F[_], A](val fa: F[A]) extends AnyVal {
    def inject[G[_] <: CopK[_, _]](implicit ev: CopK.Inject[F, G]): G[A] = ev(fa)
  }

  // ensure that everything gets injected to the right positions
  assert(Op0().inject[AlgA].index == 0)
  assert(Op0().inject[AFwd].index == 0)
  assert(Op0().inject[ARev].index == 5)
  assert(Op1().inject[AlgA].index == 1)
  assert(Op1().inject[AFwd].index == 1)
  assert(Op1().inject[ARev].index == 4)
  assert(Op2().inject[AlgA].index == 2)
  assert(Op2().inject[AFwd].index == 2)
  assert(Op2().inject[ARev].index == 3)
  assert(Op3().inject[AlgB].index == 0)
  assert(Op3().inject[AFwd].index == 3)
  assert(Op3().inject[ARev].index == 2)
  assert(Op4().inject[AlgB].index == 1)
  assert(Op4().inject[AFwd].index == 4)
  assert(Op4().inject[ARev].index == 1)
  assert(Op5().inject[AlgB].index == 2)
  assert(Op5().inject[AFwd].index == 5)
  assert(Op5().inject[ARev].index == 0)

}
