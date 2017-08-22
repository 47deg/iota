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

import org.scalacheck._

object ProdTests extends Properties("ProdTests") {
  import TList.::
  import TList.Op._

  type StringIntDoubleL = String :: Int :: Double :: TNil

  val foo0 = Prod[StringIntDoubleL]("hello", 20, 6.62607004)
  val foo1 = Prod[Reverse[StringIntDoubleL]](6.62607004, 20, "hello")

}
