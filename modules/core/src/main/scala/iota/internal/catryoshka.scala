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
package internal

import cats.Applicative
import cats.Eval
import cats.Functor
import cats.Monad
import cats.Traverse
import cats.free.Cofree
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.traverse._

/** A gross oversimplification/hack of Matryoshka, but for Cats
  * instead of Scalaz, and also tailored to the specific needs
  * of Iota.
  *
  * The vast majoriy of this file is derived/copied/modified from
  * Matryoshka, which is also licensed under the Apache License,
  * version 2.0 (and copyright SlamData Inc).
  *
  * @see https://github.com/slamdata/matryoshka
  */
private[internal] object catryoshka {

  type Algebra   [F[_], A]       = F[A] =>     A
  type AlgebraM  [M[_], F[_], A] = F[A] =>   M[A]
  type Coalgebra [F[_], A]       =   A  =>   F[A]
  type CoalgebraM[M[_], F[_], A] =   A  => M[F[A]]

  final def hylo[F[_], A, B]
    (a: A)
    (alg: Algebra[F, B], coalg: Coalgebra[F, A])
    (implicit F: Functor[F])
      : B =
    alg(F.map(coalg(a))(hylo(_)(alg, coalg)))


  final def hyloM[M[_], F[_], A, B]
    (a: A)
    (algM: AlgebraM[M, F, B], coalgM: CoalgebraM[M, F, A])
    (implicit M: Monad[M], F: Traverse[F])
      : M[B] =
    hylo[ λ[α => M[F[α]]], A, M[B] ](a)(
      fb => M.flatMap(fb)(b => M.flatMap(F.sequence(b))(algM)),
      coalgM)(M compose F)

  trait Based[T] {
    type Base[A]
  }

  trait Recursive[T] extends Based[T] { self =>
    final implicit val recursive: Recursive.Aux[T, Base] = self
    def project(t: T)(implicit BF: Functor[Base]): Base[T]

    def cata[A]
      (t: T)
      (f: Algebra[Base, A])
      (implicit BF: Functor[Base])
        : A =
      hylo(t)(f, project)

    def cataM[M[_]: Monad, A]
      (t: T)
      (f: AlgebraM[M, Base, A])
      (implicit BT: Traverse[Base])
        : M[A] =
      cata[M[A]](t)(_.sequence >>= f)

    def transCata[U, G[_]: Functor]
      (t: T)
      (f: Base[U] => G[U])
      (implicit U: Corecursive.Aux[U, G], BF: Functor[Base])
        : U =
      cata(t)(f andThen (U.embed(_)))
  }

  object Recursive {
    type Aux[T, F[_]] = Recursive[T] { type Base[A] = F[A] }
    def apply[T](implicit ev: Recursive[T]): Aux[T, ev.Base] = ev
  }

  trait Corecursive[T] extends Based[T] { self =>
    final implicit val corecursive: Corecursive.Aux[T, Base] = self
    def embed(t: Base[T])(implicit BF: Functor[Base]): T

    def ana[A](a: A)
      (f: Coalgebra[Base, A])
      (implicit BF: Functor[Base])
        : T =
      hylo(a)(embed, f)

    def anaM[M[_]: Monad, A]
      (a: A)
      (f: CoalgebraM[M, Base, A])
      (implicit BT: Traverse[Base])
        : M[T] =
      hyloM[M, Base, A, T](a)(embed(_).pure[M], f)
  }

  object Corecursive {
    type Aux[T, F[_]] = Corecursive[T] { type Base[A] = F[A] }
    def apply[T](implicit ev: Corecursive[T]): Aux[T, ev.Base] = ev
  }

  trait Birecursive[T] extends Recursive[T] with Corecursive[T]

  object Birecursive {
    type Aux[T, F[_]] = Birecursive[T] { type Base[A] = F[A] }
    def apply[T](implicit ev: Birecursive[T]): Aux[T, ev.Base] = ev

    def algebraIso[T, F[_]](
      alg: Algebra[F, T], coalg: Coalgebra[F, T]
    ): Birecursive.Aux[T, F] =
      new Birecursive[T] {
        type Base[A] = F[A]
        final def embed(ft: F[T])(implicit F: Functor[F]) = alg(ft)
        final def project(t: T)(implicit F: Functor[F]) = coalg(t)
      }
  }

  final case class Fix[F[_]](unFix: F[Fix[F]])

  object Fix {
    implicit def fixBirecursive[F[_]]: Birecursive.Aux[Fix[F], F] =
      Birecursive.algebraIso(Fix(_), _.unFix)
  }

  case class EnvT[B, W[_], A](ask: B, lower: W[A])
  object EnvT {
    implicit def envTTraverse[Z, F[_]](implicit F: Traverse[F]): Traverse[EnvT[Z, F, ?]] =
      new Traverse[EnvT[Z, F, ?]] {
        def traverse[G[_], A, B](fa: EnvT[Z, F, A])(f: A => G[B])(implicit G: Applicative[G]): G[EnvT[Z, F, B]] =
          G.map(F.traverse(fa.lower)(f))(EnvT(fa.ask, _))

        def foldLeft[A, B](fa: EnvT[Z, F, A], b: B)(f: (B, A) => B): B =
          F.foldLeft(fa.lower, b)(f)

        def foldRight[A, B](fa: EnvT[Z, F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
          F.foldRight(fa.lower, lb)(f)
      }
  }

  implicit def cofreeBirecursive[F[_], A]: Birecursive.Aux[Cofree[F, A], EnvT[A, F, ?]] =
    Birecursive.algebraIso(
      t => Cofree(t.ask, Eval.later(t.lower)),
      t => EnvT(t.head, t.tail.value))

  implicit final class AlgebraOps[F[_], A](val self: F[A] => A) extends AnyVal {
    // note: above, Algebra[F, A] is expanded to F[A] => A because this
    // apparently has better type inferencing
    def generalizeM[M[_]: Applicative](implicit F: Functor[F]): AlgebraM[M, F, A] =
      node => self(node).pure[M]
  }

  implicit final class CoalgebraOps[F[_], A](val self: Coalgebra[F, A]) extends AnyVal {
    def assign[B](b: B): Coalgebra[EnvT[B, F, ?], A] =
      a => EnvT[B, F, A](b, self(a))
  }

  implicit final class CoalgebraMOps[M[_], F[_], A](val self: CoalgebraM[M, F, A]) extends AnyVal {
    def assign[B](b: B)(implicit M: Functor[M]): CoalgebraM[M, EnvT[B, F, ?], A] =
      a => M.map(self(a))(aa => EnvT[B, F, A](b, aa))
  }

}
