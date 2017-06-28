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

/** A coproduct of type constructors captured by type constructor list `L` */
final class CopK[LL <: KList, A] private[iota](
  val index: Int,
  val value: Any
) {
  type L = LL

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: CopK[LL, A] => (index == other.index) && (value == other.value)
    case _                  => false
  }

  override def toString: String =
    s"CopK($value @ $index)"
}

object CopK {

  def unsafeApply[L <: KList, F[_], A](index: Int, fa: F[A]): CopK[L, A] =
    new CopK[L, A](index, fa)

  /** A type class witnessing the ability to inject type constructor `F`
    * into a coproduct of type constructors `G`
    */
  sealed abstract class Inject[F[_], G[_] <: CopK[_, _]] {
    def inj[A](fa: F[A]): G[A]
    def proj[A](ga: G[A]): Option[F[A]]
    final def apply[A](fa: F[A]): G[A] = inj(fa)
    final def unapply[A](ga: G[A]): Option[F[A]] = proj(ga)
  }

  object Inject {
    def apply[F[_], G[_] <: CopK[_, _]](implicit ev: Inject[F, G]): Inject[F, G] = ev

    implicit def injectFromInjectL[F[_], L <: KList](
      implicit ev: InjectL[F, L]
    ): Inject[F, CopK[L, ?]] = new Inject[F, CopK[L, ?]] {
      def inj[A](fa: F[A]): CopK[L, A] = ev.inj(fa)
      def proj[A](ca: CopK[L, A]): Option[F[A]] = ev.proj(ca)
    }
  }

  /** A type class witnessing the ability to inject type constructor `F`
    * into a coproduct of types constructors for [[KList]] type `L`
    */
  final class InjectL[F[_], L <: KList] private[InjectL](index: Int) {
    def inj[A](fa: F[A]): CopK[L, A] = new CopK[L, A](index, fa)
    def proj[A](ca: CopK[L, A]): Option[F[A]] =
      if (ca.index == index) Some(ca.value.asInstanceOf[F[A]])
      else None
    def apply[A](fa: F[A]): CopK[L, A] = inj(fa)
    def unapply[A](ca: CopK[L, A]): Option[F[A]] = proj(ca)

    type Rest = KList.Compute[KList.Op.Without[F, L]]#Out
    def projEither[A](c: CopK[L, A]): Either[CopK[Rest, A], F[A]] =
      Either.cond(
        c.index == index,
        c.value.asInstanceOf[F[A]],
        new CopK(if (c.index < index) c.index else c.index - 1, c.value))

  }

  object InjectL {
    def apply[F[_], L <: KList](implicit ev: InjectL[F, L]): InjectL[F, L] = ev
    implicit def makeInjectL[F[_], L <: KList](implicit ev: KList.Pos[L, F]): InjectL[F, L] =
      new InjectL[F, L](ev.index)

    type Aux[F[_], L <: KList, K <: KList] = InjectL[F, L] { type Rest = K }
  }

  val FunctionK: CopKFunctionK.type = CopKFunctionK
}
