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

import scala.reflect.ClassTag

package object
  iota  //#=cats
  iotaz //#=scalaz
{

  /** The terminal element of a type list */
  type TNil <: TList

  /** A type list characterized by a head type and a list of tail types
    *
    * @tparam H the head type
    * @tparam T the list of tail types
    */
  type TCons[H, T <: TList] <: TList

  /** The terminal element of a type constructor list */
  type TNilK <: TListK

  /** A type constructor list characterized by a head type
    * constructor and a list of tail type constructors
    *
    * @tparam H the head type constructor
    * @tparam T the list of tail type constructors
    */
  type TConsK[H[_], T <: TListK] <: TListK

  // -- internals --

  private[iota]  //#=cats
  private[iotaz] //#=scalaz
  type SingletonInt = Int with Singleton

  // compatability layer for cross compiling against various
  // combinations of 2.11/2.12 + cats/scalaz

  // either compat

  private[iota]  //#=cats
  private[iotaz] //#=scalaz
  implicit final class ToEitherCompatOps[A](
    val obj: A
  ) extends AnyVal {
    def asLeft[B]: Either[A, B]  = Left(obj)
    def asRight[B]: Either[B, A] = Right(obj)
  }

  //#+2.11
  private[iota]  //#=cats
  private[iotaz] //#=scalaz
  implicit final class Scala211_EitherCompatOps[A, B](
    val eab: Either[A, B]
  ) extends AnyVal {
    def flatMap[AA >: A, C](f: B => Either[AA, C]): Either[AA, C] =
      eab.right.flatMap(f)
    def map[AA >: A, C](f: B => C): Either[AA, C] =
      eab.right.map(f)
    def leftMap[C](f: A => C): Either[C, B] = eab match {
      case Left(a)      => Left(f(a))
      case r @ Right(_) => r.asInstanceOf[Either[C, B]]
    }
    def filterOrElse[AA >: A](p: B => Boolean, zero: => AA): Either[AA, B] = eab match {
      case Right(b) => if (p(b)) eab else Left(zero)
      case Left(a)  => eab
    }
  }
  //#-2.11

  //#+2.12
  //#+scalaz
  private[iotaz]
  implicit final class Scala212_Scalaz_EitherCompatOps[A, B](
    val eab: Either[A, B]
  ) extends AnyVal {
    def leftMap[C](f: A => C): Either[C, B] = eab match {
      case Left(a)      => Left(f(a))
      case r @ Right(_) => r.asInstanceOf[Either[C, B]]
    }
  }
  //#-scalaz
  //#-2.12

  // "avowal" aka validation/validated compat

  //#+cats
  import cats.NotNull
  import cats.data.Validated
  import cats.data.ValidatedNel

  type Avowal[A, B]    = Validated[A, B]
  type AvowalNel[A, B] = ValidatedNel[A, B]

  object Avowal {
    def yes  [A, B](b: B): Avowal[A, B]    = Validated.valid(b)
    def no   [A, B](a: A): Avowal[A, B]    = Validated.invalid(a)
    def noNel[A, B](a: A): AvowalNel[A, B] = Validated.invalidNel(a)

    def catching[T >: Null <: Throwable]: CatchingPartiallyApplied[T] =
      new CatchingPartiallyApplied[T]

    private[Avowal] final class CatchingPartiallyApplied[T >: Null <: Throwable] {
      def apply[A](f: => A)(implicit T: ClassTag[T], NT: NotNull[T]): Avowal[T, A] =
        Validated.catchOnly[T](f)
    }
  }
  //#-cats

  //#+scalaz
  import scalaz.NonEmptyList
  import scalaz.NotNothing
  import scalaz.Validation
  import scalaz.ValidationNel

  private[iotaz] implicit final class NonEmptyListCompanionOps(
    val companion: NonEmptyList.type
  ) extends AnyVal {
    def one[A](head: A): scalaz.NonEmptyList[A] = scalaz.NonEmptyList(head)
  }

  type Avowal[A, B]    = Validation[A, B]
  type AvowalNel[A, B] = ValidationNel[A, B]

  object Avowal {
    def yes  [A, B](b: B): Avowal[A, B]    = Validation.success(b)
    def no   [A, B](a: A): Avowal[A, B]    = Validation.failure(a)
    def noNel[A, B](a: A): AvowalNel[A, B] = Validation.failureNel(a)

    def catching[T >: Null <: Throwable]: CatchingPartiallyApplied[T] =
      new CatchingPartiallyApplied[T]

    private[Avowal] final class CatchingPartiallyApplied[T >: Null <: Throwable] {
      def apply[A](f: => A)(implicit T: ClassTag[T], NT: NotNothing[T]): Avowal[T, A] =
        Validation.fromTryCatchThrowable[A, T](f)
    }
  }
  //#-scalaz

  private[iota]  //#=cats
  private[iotaz] //#=scalaz
  implicit final class EitherToAvowalOps[A, B](
    val eab: Either[A, B]
  ) extends AnyVal {
    def toAvowal: Avowal[A, B] = eab match {
      case Left(a)  => Avowal.no(a)
      case Right(b) => Avowal.yes(b)
    }
    def toAvowalNel[AA >: A]: AvowalNel[AA, B] = eab match {
      case Left(a)  => Avowal.noNel(a)
      case Right(b) => Avowal.yes(b)
    }
  }
}
