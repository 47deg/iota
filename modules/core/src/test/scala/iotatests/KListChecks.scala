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

object KListChecks {

  import KList.Compute
  import KList.Op._
  import KList.::

  def check[L <: KList, O <: KList](implicit ev: Compute.Aux[L, O]): Unit = ()

  check[
    Reverse[Option :: List :: KNil],
    List :: Option :: KNil]

  type OptionListL = Option :: List :: KNil
  type ListOptionL = List :: Option :: KNil

  check[Reverse[OptionListL], ListOptionL]
  check[Reverse[ListOptionL], OptionListL]

  check[Concat[OptionListL, ListOptionL],
    Option :: List :: List :: Option :: KNil]

  check[Take[3, List :: Drop[1, Concat[Reverse[OptionListL], OptionListL]]],
    List :: Option :: Option :: KNil]

  type OptionList[A] = CopK[OptionListL, A]
  type ListOption[A] = CopK[ListOptionL, A]

  check[Concat[OptionList[_]#L, ListOption[_]#L],
    Option :: List :: List :: Option :: KNil]


  check[Concat[Option :: KNil, List :: KNil], OptionListL]

  check[Concat[Reverse[OptionListL], OptionListL],
    List :: Option :: Option :: List :: KNil]

  check[Drop[1, OptionListL], List :: KNil]

  check[Remove[Option, OptionListL], List :: KNil]
  check[Remove[List, OptionListL], Option :: KNil]
  check[Remove[List, List :: List :: KNil], List :: KNil]

}
