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

/** A coproduct of types captured by type list `L` */
final class Cop[LL <: TList] private[iota](
  val index: Int,
  val value: Any
) {
  type L = LL

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: Cop[LL] => (index == other.index) && (value == other.value)
    case _              => false
  }

  override def toString: String =
    s"Cop($value @ $index)"
}

object Cop {

  def unsafeApply[L <: TList, A](index: Int, a: A): Cop[L] =
    new Cop[L](index, a)

  /** A type class witnessing the ability to inject type `A` into a
    * coproduct of types `B`
    */
  sealed abstract class Inject[A, B <: Cop[_]] extends cats.Inject[A, B]

  object Inject {
    def apply[A, B <: Cop[_]](implicit ev: Inject[A, B]): Inject[A, B] = ev

    implicit def injectFromInjectL[A, L <: TList](
      implicit ev: InjectL[A, L]
    ): Inject[A, Cop[L]] = new Inject[A, Cop[L]] {
      val inj: A => Cop[L] = ev.inj(_)
      val prj: Cop[L] => Option[A] = ev.proj(_)
    }
  }

  /** A type class witnessing the ability to inject type `A` into a
    * coproduct of types for [[TList]] type `L`
    */
  final class InjectL[A, L <: TList] private[InjectL](index: Int) {
    def inj(a: A): Cop[L] = new Cop[L](index, a)
    def proj(c: Cop[L]): Option[A] =
      if (c.index == index) Some(c.value.asInstanceOf[A])
      else None
    def apply(a: A): Cop[L] = inj(a)
    def unapply(c: Cop[L]): Option[A] = proj(c)
  }

  object InjectL {
    def apply[A, L <: TList](implicit ev: InjectL[A, L]): InjectL[A, L] = ev
    implicit def makeInjectL[A, L <: TList](implicit ev: TList.Pos[L, A]): InjectL[A, L] =
      new InjectL[A, L](ev.index)
  }

  final class RemoveL[A, L <: TList] private[RemoveL](index: Int) {
    def apply(c: Cop[L]): Either[Cop[TList.Op.Remove[A, L]], A] =
      Either.cond(
        c.index == index,
        c.value.asInstanceOf[A],
        new Cop(if (c.index < index) c.index else c.index - 1, c.value))
  }

  object RemoveL {
    def apply[A, L <: TList](implicit ev: RemoveL[A, L]): RemoveL[A, L] = ev
    implicit def makeRemoveL[A, L <: TList](implicit ev: TList.Pos[L, A]): RemoveL[A, L] =
      new RemoveL[A, L](ev.index)
  }

}
