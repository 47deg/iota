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

package iotatests

import iota._

object FooAndFriends {

  import TList.::

  trait Foo
  type Bar = String
  type Baz = Long with ("t", "t", "o", "c", "s")

  type FooBarBazL = Foo :: Bar :: Baz :: TNil
  type BazBarFooL = Baz :: Bar :: Foo :: TNil

  type FooBarBaz = Cop[FooBarBazL]
  type BazBarFoo = Cop[BazBarFooL]


  import TListK.:::

  trait FooK[A]
  type BarK[A] = cats.data.NonEmptyList[A]
  type BazK[A] = Option[String with A] // ¯\_(ツ)_/¯

  type FooBarBazKL = FooK ::: BarK ::: BazK ::: TNilK
  type BazBarFooKL = BazK ::: BarK ::: FooK ::: TNilK

  type FooBarBazK[A] = CopK[FooBarBazKL, A]
  type BazBarFooK[A] = CopK[BazBarFooKL, A]

}
