package iotatests

import cats._
import iota._

object CopKFunctionKTests {

  import TListK.::

  type FooOp[A] = A with "foo"
  type BarOp[A] = A with "bar"

  implicit val interpFooOp: FooOp ~> Either[String, ?] =
    new (FooOp ~> Either[String, ?]) {
      def apply[A](fa: FooOp[A]): Either[String, A] = Right(fa)
    }

  implicit val interpBarOp: BarOp ~> Either[String, ?] =
    new (BarOp ~> Either[String, ?]) {
      def apply[A](fa: BarOp[A]): Either[String, A] = Right(fa)
    }

  CopKFunctionK.summon[CopK[FooOp :: BarOp :: TNilK, ?], Either[String, ?]]

}
