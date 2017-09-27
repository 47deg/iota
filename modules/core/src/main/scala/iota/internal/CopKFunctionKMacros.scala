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
package internal

import cats._
import cats.data._
import cats.instances.all._
import cats.syntax.either._ //#=2.12

import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.TypecheckException

final class CopKFunctionKMacros(val c: Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  def of[F[a] <: CopK[_, a], G[_]](args: c.Expr[Any]*)(
    implicit
      evF: c.WeakTypeTag[F[_]],
      evG: c.WeakTypeTag[G[_]]
  ): c.Expr[F ~> G] = {

    val F = evF.tpe
    val G = evG.tpe

    tb.foldAbort(for {
      copK <- tb.destructCopK(F).leftMap(NonEmptyList.one(_))
      tpes <- tb.memoizedTListKTypes(copK.L).leftMap(NonEmptyList.one(_))

      unorderedPairs <- Traverse[List].traverse(args.toList)(arg =>
        destructFunctionKInput(arg.tree.tpe, G).map((_, arg.tree))).toEither
      lookup = unorderedPairs.toMap

      arrs <- Traverse[List].traverse(tpes)(tpe =>
        lookup.get(tpe).toRight(s"Missing interpreter FunctionK[$tpe, $G]").toValidatedNel).toEither
    } yield makeInterpreter(F, copK.L, G, arrs))
  }

  def summon[F[a] <: CopK[_, a], G[_]](
    implicit
      evF: c.WeakTypeTag[F[_]],
      evG: c.WeakTypeTag[G[_]]
  ): c.Expr[F ~> G] = {

    val F = evF.tpe
    val G = evG.tpe

    tb.foldAbort(for {
      copK <- tb.destructCopK(F).leftMap(NonEmptyList.one(_))
      tpes <- tb.memoizedTListKTypes(copK.L).leftMap(NonEmptyList.one(_))

      arrs <- Traverse[List].traverse(tpes)(tpe =>
                summonFunctionK(tpe, G)).toEither
    } yield makeInterpreter(F, copK.L, G, arrs))
  }

  private[this] def makeInterpreter(
    F: Type,
    L: Type,
    G: Type,
    arrs: List[Tree]
  ): Tree = {

    val handlers = arrs.zipWithIndex.map { case (arr, i) =>
      val name = TermName(s"arr$i")
      val pre = q"private[this] val $name = $arr.asInstanceOf[_root_.cats.arrow.FunctionK[Any, $G]]"
      val handler = (fa: TermName) => q"$name($fa.value)"
      (pre, handler)
    }

    val name = TypeName(c.freshName("CopKFunctionK"))
    val defn = tb.defineFastFunctionK(
      name, F, G,
      preamble = handlers.map(_._1),
      toIndex  = fa => q"$fa.index",
      handlers = handlers.map(_._2))

    q"""$defn; new $name"""
  }

  private[this] def summonFunctionK(F: Type, G: Type): ValidatedNel[String, Tree] =
    Validated
      .catchOnly[TypecheckException](
        c.typecheck(q"_root_.scala.Predef.implicitly[_root_.cats.arrow.FunctionK[$F, $G]]"))
      .leftMap(t => NonEmptyList.one(t.msg))

  private[this] def destructFunctionKInput(tpe: Type, G: Type): ValidatedNel[String, Type] =
    tpe match {
      case TypeRef(_, sym, f :: g :: Nil) if g =:= G => Validated.valid(f)
      case RefinedType(anyRef :: tpe2 :: Nil, scope) => // TODO: check anyRef is scala.AnyRef
        destructFunctionKInput(tpe2.dealias, G)
      case _ =>
        Validated.invalidNel(s"unable to destruct input $tpe as FunctionK[?, $G]\n" +
          s"  underlying type tree: ${showRaw{tpe}} (class ${tpe.getClass})")
    }
}
