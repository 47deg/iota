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

import cats.data._ //#=2.11

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

  //#+2.11
  private[iota] implicit final class EitherCompatOps[A, B](
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
    def toValidated: Validated[A, B] = eab match {
      case Left(a)  => Validated.invalid(a)
      case Right(b) => Validated.valid(b)
    }
    def toValidatedNel[AA >: A]: ValidatedNel[AA, B] = eab match {
      case Left(a)  => Validated.invalidNel(a)
      case Right(b) => Validated.valid(b)
    }
    def filterOrElse[AA >: A](p: B => Boolean, zero: => AA): Either[AA, B] = eab match {
      case Right(b) => if (p(b)) eab else Left(zero)
      case Left(a)  => eab
    }
  }

  private[iota] implicit final class ToEitherCompatOps[A](
    val obj: A
  ) extends AnyVal {
    def asLeft[B]: Either[A, B]  = Left(obj)
    def asRight[B]: Either[B, A] = Right(obj)
  }
  //#-2.11

  private[iota]  //#=cats
  private[iotaz] //#=scalaz
  type SingletonInt = Int with Singleton
}
