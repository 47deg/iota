package iota  //#=cats
package iotaz //#=scalaz
package evidence

final class FirstK[L <: TListK, A](val underlying: CopK[L, A]) extends AnyVal

object FirstK {
  def apply[L <: TListK, A](implicit ev: FirstK[L, A]): FirstK[L, A] = ev

  implicit def materializeFirstK[L <: TListK, A]: FirstK[L, A] =
    macro internal.EvidenceMacros.materializeFirstK[L, A]

}
