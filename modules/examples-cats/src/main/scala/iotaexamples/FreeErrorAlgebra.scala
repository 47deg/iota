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

package iotaexamples

import cats.{~>, ApplicativeError, Monad}
import cats.free.Free
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.applicativeError._
import iota._
import iota.CopK.Inject

object ErrorProvider {
  def apply[E]: ErrorProvider[E] = new ErrorProvider[E]
}

class ErrorProvider[E] {

  /**
   * Error handling algebra.
   *
   * We wrap the algebra in an `ErrorProvider[E]` class to have one `E` type 
   * for both `Error` and `Recover`.
   */
  sealed abstract class ErrorHandling[A] extends Product with Serializable
  case class Error[A](e: E) extends ErrorHandling[A]
  case class Recover[CP[_], A](fa: Free[CP, A], f: E => Free[CP, A]) extends ErrorHandling[A]

  /**
   * `ApplicativeError` instance for `Free[CP, ?]` where `CP` is a `CopK` that
   * contains the `ErrorHandling` algebra.
   */
  implicit def applicativeErrorFreeWithErrorHandling[CP[_] <: CopK[_, _]](
    implicit inj: Inject[ErrorHandling, CP]
  ): ApplicativeError[Free[CP, ?], E] = 
    new ApplicativeError[Free[CP, ?], E]{
      def pure[A](a: A):       Free[CP, A] = Free.pure(a)
      def raiseError[A](e: E): Free[CP, A] = Free.liftF(inj.inj(Error[A](e)))
      def handleErrorWith[A](fa: Free[CP, A])(f: E => Free[CP, A]): Free[CP, A] =
        Free.liftF(inj.inj(Recover(fa, f)))
      def ap[A, B](ff: Free[CP, A => B])(fa: Free[CP, A]): Free[CP, B] =
        ff.flatMap(f => fa.map(f))
    }

  /**
   * An interpreter for a `CopK[KL, ?]` where `KL` contains the `ErrorHandling` algebra.
   * 
   * Using the `CopK.RemoveL` type class we can split a coproduct into either 
   * an `ErrorHandling` operation or a coproduct without `ErrorHandling` operations.
   *
   * If we have an interpreter for all the other algebras in the coproduct, we can use it
   * to interpret the coproduct with `ErrorHandling` operations and use an `ApplicativeError`
   * instance for the result type to interpret the `ErrorHandling` operations.
   *
   * Note that the interpretation of `ErrorHandling` is not stacksafe, so nesting a lot of
   * `handleErrorWith` methods could lead to a stack overflow.
   */
  implicit def handler[KL <: TListK, M[_]](
    implicit
      M: Monad[M],
      ME: ApplicativeError[M, E],
      removeL: CopK.RemoveL[ErrorHandling, KL],
      restHandler: CopK[TListK.Op.Remove[ErrorHandling, KL], ?] ~> M
  ): CopK[KL, ?] ~> M = 
    new (CopK[KL, ?] ~> M) {
      def apply[A](c: CopK[KL, A]): M[A] = 
        removeL(c) match {
          case Right(errorOp) =>
            errorOp match {
              case Error(e) => ME.raiseError(e)
              case r @ Recover(_, _) =>
                val Recover(fa, f) = r.asInstanceOf[Recover[CopK[KL, ?], A]]
                ME.handleErrorWith(fa.foldMap(this))(f(_).foldMap(this))
            }
          case Left(rest) =>
            Free.liftF(rest).foldMap(restHandler)
        }
    }

  /** Summon an interpreter for a CopK coproduct implicitly (a la freestyle) */
  implicit def interpretIotaCopK[F[a] <: CopK[_, a], G[_]]: F ~> G =
    macro iota.internal.CopKFunctionKMacros.summon[F, G]
}

object FreeErrorAlgebra {

  // other algebras
  sealed abstract class FooOp[A] extends Product with Serializable
  final case class Foo(i: Int) extends FooOp[Int]

  sealed abstract class BarOp[A] extends Product with Serializable
  final case class Bar(s: String) extends BarOp[Int]

  // using String as error type
  val stringError = ErrorProvider[String]
  import stringError._

  // combined coproduct
  import iota.TListK.:::
  type FooBarErrorOpL = FooOp ::: BarOp ::: ErrorHandling ::: TNilK
  type FBE[A] = CopK[FooBarErrorOpL, A]

  // smart constructors
  def foo(i: Int   ): Free[FBE, Int] = Free.liftF(CopK.Inject[FooOp, FBE].inj(Foo(i)))
  def bar(s: String): Free[FBE, Int] = Free.liftF(CopK.Inject[BarOp, FBE].inj(Bar(s)))

  // example program
  val example: Free[FBE, Int] = foo(1).handleErrorWith(e => bar("hello"))

  // handlers simple algebras
  implicit val fooHandler = λ[FooOp ~> Either[String, ?]]{ case Foo(i) => Either.left("kaboom") }
  implicit val barHandler = λ[BarOp ~> Either[String, ?]]{ case Bar(s) => Either.right(s.length) }

  // mimick FreeS.interpret
  implicit class FreeInterpret[F[_], A](val fa: Free[F, A]) extends AnyVal {
    def interpret[M[_]: Monad](implicit fk: F ~> M): M[A] = fa.foldMap(fk)
  }
  
  def main(args: Array[String]): Unit = {
    val result = example.interpret[Either[String, ?]]
    scala.Predef.assert(result == Right(5))
    scala.Predef.println("result = " + result.toString)
  }
}
