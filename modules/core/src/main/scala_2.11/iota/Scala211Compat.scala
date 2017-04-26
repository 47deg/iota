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

import cats.data._

private[iota] class Scala211Compat {
  private[iota] implicit final class EitherCompatOps[A, B](eab: Either[A, B]) {
    def flatMap[AA >: A, C](f: B => Either[AA, C]): Either[AA, C] =
      eab.right.flatMap(f)
    def map[AA >: A, C](f: B => C): Either[AA, C] =
      eab.right.map(f)
    def leftMap[C](f: A => C): Either[C, B] = eab match {
      case Left(a)      => Left(f(a))
      case r @ Right(_) => r.asInstanceOf[Either[C, B]]
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
}
