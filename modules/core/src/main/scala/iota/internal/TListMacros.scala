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

import scala.unchecked
import scala.reflect.macros.blackbox.Context
import scala.annotation.tailrec

import scala.collection.immutable.Map

private[internal] object TListMacros {
  @volatile var klistCache: Map[Any, Any] = Map.empty
}

class TListMacros(val c: Context) {
  import c.universe._

  def materializePos[L <: TList, A](
    implicit
      evL: c.WeakTypeTag[L],
      evA: c.WeakTypeTag[A]
  ): c.Expr[TList.Pos[L, A]] = {

    val L = evL.tpe.dealias
    val A = evA.tpe.dealias

    result(for {
      algebras <- klistTypesCached(L)
      index    <- Right(algebras.indexWhere(_.dealias == A))
                    .filterOrElse(_ >= 0, s"$A is not a member of $L")
    } yield
      q"new TList.Pos[$L, $A]{ override val index: Int = $index }")
  }

  def result[T](either: Either[String, Tree]): c.Expr[T] =
    either fold (
      error => c.abort(c.enclosingPosition, error),
      tree  => c.Expr[T](tree))

  private[this] val TNilSym          = typeOf[TNil].typeSymbol
  private[this] val TConsSym         = typeOf[TCons[Nothing, Nothing]].typeSymbol

  private[this] lazy val showCache =
    !c.inferImplicitValue(typeOf[debug.optionTypes.ShowCache], true).isEmpty

  @tailrec
  private[this] final def klistFoldLeft[A](tpe: Type)(a0: A)(f: (A, Type) => A): Either[String, A] = tpe match {
    case TypeRef(_, TNilSym, Nil) => Right(a0)
    case TypeRef(_, cons, List(headType, tailType)) if cons.asType.toType.contains(TConsSym) =>
      klistFoldLeft(tailType)(f(a0, headType))(f)
    case _ =>
      Left(s"Unexpected type ${showRaw(tpe)} when inspecting HList")
  }

  private[this] final def klistTypes(tpe: Type): Either[String, List[Type]] =
    klistFoldLeft(tpe)(List.empty[Type])((acc, t) => t :: acc).map(_.reverse)

  private[this] final def klistTypesCached(
    tpe: Type
  ): Either[String, List[Type]] = TListMacros.klistCache.synchronized {
    TListMacros.klistCache.get(tpe) match {
      case Some(res: Either[String, List[Type]] @unchecked) =>
        if (showCache)
          c.echo(c.enclosingPosition, s"ShowCache(TList): $tpe cached result $res")
        res
      case _ =>
        val res = klistTypes(tpe)
        TListMacros.klistCache += ((tpe, res))
        if (showCache)
          c.echo(c.enclosingPosition, s"ShowCache(TList): $tpe computed result $res")
        res
    }
  }
}
