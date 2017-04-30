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

class KListMacros(val c: Context) {
  import c.universe._

  val klists = new SharedKListMacros[c.type](c)

  private[this] lazy val showAborts =
    !c.inferImplicitValue(typeOf[debug.optionTypes.ShowAborts], true).isEmpty

  def materializePos[L <: KList, F[_]](
    implicit
      evL: c.WeakTypeTag[L],
      evF: c.WeakTypeTag[F[_]]
  ): c.Expr[KList.Pos[L, F]] = {

    val L = evL.tpe.dealias
    val F = evF.tpe.dealias

    result(for {
      algebras <- klists.klistTypesCached(L)
      index    <- Right(algebras.indexWhere(_ =:= F))
                    .filterOrElse(_ >= 0, s"$F is not a member of $L")
    } yield
      q"new _root_.iota.KList.Pos[$L, $F]{ override val index: Int = $index }")
  }

  def result[T](either: Either[String, Tree]): c.Expr[T] =
    either fold (
      error => {
        if (showAborts) c.echo(c.enclosingPosition, error)
        c.abort(c.enclosingPosition, error)
      },
      tree  => c.Expr[T](tree))

}

private[internal] object SharedKListMacros {
  @volatile var klistCache: Map[Any, Any] = Map.empty
}

private[internal] class SharedKListMacros[C <: Context](val c: C) {
  import c.universe._

  private[this] val KNilSym          = typeOf[KNil].typeSymbol
  private[this] val KConsSym         = typeOf[KCons[Nothing, Nothing]].typeSymbol

  private[this] lazy val showCache =
    !c.inferImplicitValue(typeOf[debug.optionTypes.ShowCache], true).isEmpty

  @tailrec
  private[this] final def klistFoldLeft[A](tpe: Type)(a0: A)(f: (A, Type) => A): Either[String, A] = tpe.dealias match {
    case TypeRef(_, KNilSym, Nil) => Right(a0)
    case TypeRef(_, cons, List(headType, tailType)) if cons.asType.toType.contains(KConsSym) =>
      klistFoldLeft(tailType)(f(a0, headType))(f)
    case _ =>
      Left(s"Unexpected type ${showRaw(tpe)} when inspecting KList")
  }

  private[this] final def klistTypes(tpe: Type): Either[String, List[Type]] =
    klistFoldLeft(tpe)(List.empty[Type])((acc, t) => t :: acc).map(_.reverse)

  final def klistTypesCached(
    tpe: Type
  ): Either[String, List[Type]] = SharedKListMacros.klistCache.synchronized {
    SharedKListMacros.klistCache.get(tpe) match {
      case Some(res: Either[String, List[Type]] @unchecked) =>
        if (showCache)
          c.echo(c.enclosingPosition, s"ShowCache(KList): $tpe cached result $res")
        res
      case _ =>
        val res = klistTypes(tpe)
        SharedKListMacros.klistCache += ((tpe, res))
        if (showCache)
          c.echo(c.enclosingPosition, s"ShowCache(KList): $tpe computed result $res")
        res
    }
  }

}
