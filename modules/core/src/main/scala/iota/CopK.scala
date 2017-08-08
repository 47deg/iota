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

import cats.~>

/** A coproduct of type constructors captured by type constructor list `L` */
final class CopK[LL <: TListK, A] private[iota](
  val index: Int,
  val value: Any
) {
  type L = LL

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: CopK[LL, A] => (index == other.index) && (value == other.value)
    case _                => false
  }

  override def toString: String =
    s"CopK($value @ $index)"
}

object CopK {

  def unsafeApply[L <: TListK, F[_], A](index: Int, fa: F[A]): CopK[L, A] =
    new CopK[L, A](index, fa)

  /** A type class witnessing the ability to inject type constructor `F`
    * into a coproduct of type constructors `G`
    */
  sealed abstract class Inject[F[_], G[_] <: CopK[_, _]] extends cats.InjectK[F, G]

  object Inject {
    def apply[F[_], G[_] <: CopK[_, _]](implicit ev: Inject[F, G]): Inject[F, G] = ev

    implicit def injectFromInjectL[F[_], L <: TListK](
      implicit ev: InjectL[F, L]
    ): Inject[F, CopK[L, ?]] = new Inject[F, CopK[L, ?]] {
      val inj: F ~> CopK[L, ?] = λ[F ~> CopK[L, ?]](ev.inj(_))
      val prj: CopK[L, ?] ~> λ[α => Option[F[α]]] = λ[CopK[L, ?] ~> λ[α => Option[F[α]]]](ev.proj(_))
    }
  }

  /** A type class witnessing the ability to inject type constructor `F`
    * into a coproduct of types constructors for [[TListK]] type `L`
    */
  final class InjectL[F[_], L <: TListK] private[InjectL](index: Int) {
    def inj[A](fa: F[A]): CopK[L, A] = new CopK[L, A](index, fa)
    def proj[A](ca: CopK[L, A]): Option[F[A]] =
      if (ca.index == index) Some(ca.value.asInstanceOf[F[A]])
      else None
    def apply[A](fa: F[A]): CopK[L, A] = inj(fa)
    def unapply[A](ca: CopK[L, A]): Option[F[A]] = proj(ca)
  }

  object InjectL {
    def apply[F[_], L <: TListK](implicit ev: InjectL[F, L]): InjectL[F, L] = ev
    implicit def makeInjectL[F[_], L <: TListK](implicit ev: TListK.Pos[L, F]): InjectL[F, L] =
      new InjectL[F, L](ev.index)
  }

  final class RemoveL[F[_], L <: TListK] private[RemoveL](index: Int) {
    def apply[A](c: CopK[L, A]): Either[CopK[TListK.Op.Remove[F, L], A], F[A]] =
      Either.cond(
        c.index == index,
        c.value.asInstanceOf[F[A]],
        new CopK(if (c.index < index) c.index else c.index - 1, c.value))
  }

  object RemoveL {
    def apply[F[_], L <: TListK](implicit ev: RemoveL[F, L]): RemoveL[F, L] = ev
    implicit def makeRemoveL[F[_], L <: TListK](implicit ev: TListK.Pos[L, F]): RemoveL[F, L] =
      new RemoveL[F, L](ev.index)
  }

  val FunctionK: CopKFunctionK.type = CopKFunctionK
}
