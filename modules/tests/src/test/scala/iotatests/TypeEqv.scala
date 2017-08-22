package iotatests

import cats._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import shapeless.{ Id => _, _ }
import shapeless.ops.hlist.{ ToList => HListToList }

import scala.reflect.runtime.universe._

sealed trait TypeEqv[A] {
  def check(x: A, y: A): Prop
}

object TypeEqv extends TypeEqvInstances0 {
  object syntax {
    final implicit class TypeEqvOps[A](x: A)(implicit eqv: TypeEqv[A]) {
      def ?=:=(y: A): Prop = eqv.check(x, y)
    }
  }
}

sealed class TypeEqvInstances0 extends TypeEqvInstances1 {
  implicit def idTypeEqv[A <: Type]: TypeEqv[A] = new TypeEqv[A] {
    def check(x: A, y: A): Prop =
      Prop(x =:= y) :| s"$x was not =:= to $y"
  }

  implicit def eitherTypeEqv[A, B](
    implicit eqv: TypeEqv[B]
  ): TypeEqv[Either[A, B]] = new TypeEqv[Either[A, B]] {
    def check(ex: Either[A, B], ey: Either[A, B]): Prop =
      (ex, ey) match {
        case (Right(bx), Right(by)) => eqv.check(bx, by)
        case _                      => ex ?= ey
      }
  }
}

sealed class TypeEqvInstances1 {
  implicit def foldableTypeEqv[F[_], A](
    implicit F: Foldable[F], eqv: TypeEqv[A]
  ): TypeEqv[F[A]] = new TypeEqv[F[A]] {
    def check(fx: F[A], fy: F[A]): Prop =
      (F.toList(fx) zip F.toList(fy)).foldLeft(proved)((acc, vv) =>
        acc && eqv.check(vv._1, vv._2))
  }

  implicit def genericTypeEqv[P, L <: HList, A](
    implicit
      gen: Generic.Aux[P, L],
      toList: HListToList[L, A],
      eqv: TypeEqv[List[A]]
  ): TypeEqv[P] = new TypeEqv[P] {
    def check(x: P, y: P): Prop =
      eqv.check(toList(gen.to(x)), toList(gen.to(y)))
  }
}
