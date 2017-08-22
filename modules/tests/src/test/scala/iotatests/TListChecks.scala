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

object TListChecks {

  import TList.Compute
  import TList.Op._
  import TList.::

  def check[L <: TList, O <: TList](implicit ev: Compute.Aux[L, O]): Unit = ()

  check[
    Reverse[String :: Double :: TNil],
    Double :: String :: TNil]

  type StringIntL = String :: Int :: TNil
  type IntStringL = Int :: String :: TNil

  check[Reverse[StringIntL], IntStringL]
  check[Reverse[IntStringL], StringIntL]

  check[Concat[StringIntL, IntStringL],
    String :: Int :: Int :: String :: TNil]

  check[Take[3, Double :: Drop[1, Concat[Reverse[StringIntL], StringIntL]]],
    Double :: String :: String :: TNil]

  type StringInt = Cop[StringIntL]
  type IntString = Cop[IntStringL]

  check[Concat[StringInt#L, IntString#L],
    String :: Int :: Int :: String :: TNil]

  check[Remove[String, StringIntL], Int :: TNil]
  check[Remove[Int, StringIntL], String :: TNil]
  check[Remove[Int, Int :: Int :: TNil], Int :: TNil]

}
