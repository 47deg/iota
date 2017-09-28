package iotatests

import cats._   //#=cats
import scalaz._ //#=scalaz

import iota._  //#=cats
import iotaz._ //#=scalaz

object CopKFunctionKTests {
  import TestSingletonLiterals._

  import TListK.::

  type FooOp[A] = A with `"foo"`
  type BarOp[A] = A with `"bar"`

  implicit val interpFooOp: FooOp ~> Either[String, ?] =
    new (FooOp ~> Either[String, ?]) {
      def apply[A](fa: FooOp[A]): Either[String, A] = Right(fa)
    }

  implicit val interpBarOp: BarOp ~> Either[String, ?] =
    new (BarOp ~> Either[String, ?]) {
      def apply[A](fa: BarOp[A]): Either[String, A] = Right(fa)
    }

  CopKNT.summon[CopK[FooOp :: BarOp :: TNilK, ?], Either[String, ?]]
}
