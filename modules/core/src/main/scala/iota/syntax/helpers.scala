package iota  //#=cats
package iotaz //#=scalaz
package syntax

final class InjectOps[A](val a: A) extends AnyVal {
  def inject[B <: Cop[_]](implicit ev: Cop.Inject[A, B]): B =
    ev.inj(a)
}

trait InjectSyntax {
  implicit def toInjectOps[A](a: A): InjectOps[A] = new InjectOps(a)
}

final class InjectKOps[F[_], A](val fa: F[A]) extends AnyVal {
  def injectK[G[_] <: CopK[_, _]](implicit ev: CopK.Inject[F, G]): G[A] =
    ev.inj(fa)
}

trait InjectKSyntax {
  implicit def toInjectKOps[F[_], A](fa: F[A]): InjectKOps[F, A] = new InjectKOps(fa)
}
