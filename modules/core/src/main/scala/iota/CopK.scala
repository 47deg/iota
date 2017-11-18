package iota  //#=cats
package iotaz //#=scalaz

import cats.~>   //#=cats
import scalaz.~> //#=scalaz

/** A coproduct of type constructors captured by type constructor list `L` */
final class CopK[LL <: TListK, A] private(
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

  def unsafeApply[L <: TListK, F[_], A](index: Int, fa: F[A]): CopK[L, A] =
    new CopK[L, A](index, fa)

  /** A type class witnessing the ability to inject type constructor `F`
    * into a coproduct of type constructors `G`
    */
  sealed abstract class Inject[F[_], G[_] <: CopK[_, _]]
      extends cats.InjectK[F, G] //#=cats
  {
    //#+scalaz
    def inj: F ~> G
    def prj: G ~> λ[α => Option[F[α]]]
    final def apply[A](fa: F[A]): G[A] = inj(fa)
    final def unapply[A](ga: G[A]): Option[F[A]] = prj(ga)
    //#-scalaz
  }

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

  val FunctionK: CopKFunctionK.type = CopKFunctionK           //#=cats
  val NaturalTransformation: CopKNaturalTransformation.type = //#=scalaz
    CopKNaturalTransformation                                 //#=scalaz
}
