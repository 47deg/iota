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

import scala.collection.immutable.Seq

/** A product of types captured by type list `LL` */
final class Prod[LL <: TList] private(
  val values: Seq[Any]
) {
  type L = LL

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: Prod[LL] => values == other.values
    case _               => false
  }

  override def toString: String =
    s"""Prod(${values.mkString(", ")})"""
}

object Prod {

  def apply[L <: TList](args: Any*): Prod[L] =
    macro internal.ProductMacros.prodApply[L]

  def unsafeApply[L <: TList](values: Seq[Any]): Prod[L] =
    new Prod[L](values)

}
