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

object TListKChecks {

  import TListK.Compute
  import TListK.Op._
  import TListK.::

  def check[L <: TListK, O <: TListK](implicit ev: Compute.Aux[L, O]): Unit = ()

  check[
    Reverse[Option :: List :: TNilK],
    List :: Option :: TNilK]

  type OptionListL = Option :: List :: TNilK
  type ListOptionL = List :: Option :: TNilK

  check[Reverse[OptionListL], ListOptionL]
  check[Reverse[ListOptionL], OptionListL]

  check[Concat[OptionListL, ListOptionL],
    Option :: List :: List :: Option :: TNilK]

  check[Take[3, List :: Drop[1, Concat[Reverse[OptionListL], OptionListL]]],
    List :: Option :: Option :: TNilK]

  type OptionList[A] = CopK[OptionListL, A]
  type ListOption[A] = CopK[ListOptionL, A]

  check[Concat[OptionList[_]#L, ListOption[_]#L],
    Option :: List :: List :: Option :: TNilK]


  check[Concat[Option :: TNilK, List :: TNilK], OptionListL]

  check[Concat[Reverse[OptionListL], OptionListL],
    List :: Option :: Option :: List :: TNilK]

  check[Drop[1, OptionListL], List :: TNilK]

  check[Remove[Option, OptionListL], List :: TNilK]
  check[Remove[List, OptionListL], Option :: TNilK]
  check[Remove[List, List :: List :: TNilK], List :: TNilK]

}
