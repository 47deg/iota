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
package internal

import scala.reflect.macros.blackbox.Context

import cats.Traverse
import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.either._ //#=2.12

private[iota] final class ProductMacros(val c: Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  def prodApply[L <: TList](args: c.Expr[Any]*)(
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[Prod[L]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListTypes(L).leftMap(NonEmptyList.of(_))
      argTypes  = args.toList.map(_.tree.tpe)
      _        <- require(argTypes.length == algebras.length,
                    s"Expected ${algebras.length} arguments but received ${argTypes.length}")
      _        <- Traverse[List].traverse(argTypes zip algebras)(tpes =>
                    require(tpes._1 <:< tpes._2,
                      s"Expected ${tpes._1} <:< ${tpes._2}").toValidated).toEither
    } yield
      q"_root_.iota.Prod.unsafeApply[$L](_root_.scala.collection.immutable.IndexedSeq[_root_.scala.Any](..$args))")
  }

  private[this] def require(flag: Boolean, msg: => String): Either[NonEmptyList[String], Unit] =
    Either.cond(flag, (), msg).leftMap(NonEmptyList.of(_))

}
