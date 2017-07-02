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

//#+jvm

package iotatests

import iota._
import iota.internal.IotaReflectiveToolbelt

import cats.instances.all._
import scala.reflect.runtime.{ universe => runtimeUniverse }

import org.scalacheck._

class IotaReflectiveToolbeltTests extends Properties("IotaReflectiveToolbelt") {
  import TypeEqv.syntax._
  import FooAndFriends._

  import runtimeUniverse.{ Type, TypeTag, WeakTypeTag }

  def typeOfK[F[_]](implicit evF: TypeTag[F[Nothing]]): Type = evF.tpe.etaExpand.resultType
  def weakTypeOfK[F[_]](implicit evF: WeakTypeTag[F[Nothing]]): Type = evF.tpe.etaExpand.resultType

  val tb = IotaReflectiveToolbelt(runtimeUniverse)
  import runtimeUniverse.{ weakTypeOf, typeOf }

  property("tlistTypes FooBarBazL") =
    tb.tlistTypes(weakTypeOf[FooBarBazL]) ?=:= Right(
      typeOf[Foo] :: typeOf[Bar] :: typeOf[Baz] :: Nil)

  property("tlistTypes BazBarFooL") =
    tb.tlistTypes(weakTypeOf[BazBarFooL]) ?=:= Right(
      typeOf[Baz] :: typeOf[Bar] :: typeOf[Foo] :: Nil)

  property("tlistkTypeConstructors FooBarBazKL") =
    tb.tlistkTypeConstructors(weakTypeOf[FooBarBazKL]) ?=:= Right(
      typeOfK[FooK] :: typeOfK[BarK] :: typeOfK[BazK] :: Nil)

  property("tlistkTypeConstructors BazBarFooKL") =
    tb.tlistkTypeConstructors(weakTypeOf[BazBarFooKL]) ?=:= Right(
      typeOfK[BazK] :: typeOfK[BarK] :: typeOfK[FooK] :: Nil)

  property("destructCop Cop[FooBarBazL]") =
    tb.destructCop(weakTypeOf[Cop[FooBarBazL]]) ?=:= Right(tb.CopTypes(weakTypeOf[FooBarBazL]))

  property("destructCop FooBarBaz") =
    tb.destructCop(weakTypeOf[FooBarBaz]) ?=:= Right(tb.CopTypes(weakTypeOf[FooBarBazL]))

  property("destructCop Cop[BazBarFooL]") =
    tb.destructCop(weakTypeOf[Cop[BazBarFooL]]) ?=:= Right(tb.CopTypes(weakTypeOf[BazBarFooL]))

  property("destructCop BazBarFoo") =
    tb.destructCop(weakTypeOf[BazBarFoo]) ?=:= Right(tb.CopTypes(weakTypeOf[BazBarFooL]))

  property("destructCopK CopK[FooBarBazKL]") =
    tb.destructCopK(weakTypeOf[CopK[FooBarBazKL, Int]]) ?=:= Right(tb.CopKTypes(weakTypeOf[FooBarBazKL], typeOf[Int]))

  property("destructCopK FooBarBazK") =
    tb.destructCopK(weakTypeOf[FooBarBazK[Int]]) ?=:= Right(tb.CopKTypes(weakTypeOf[FooBarBazKL], typeOf[Int]))

  property("destructCopK CopK[BazBarFooKL]") =
    tb.destructCopK(weakTypeOf[CopK[BazBarFooKL, Int]]) ?=:= Right(tb.CopKTypes(weakTypeOf[BazBarFooKL], typeOf[Int]))

  property("destructCopK BazBarFooK") =
    tb.destructCopK(weakTypeOf[BazBarFooK[Int]]) ?=:= Right(tb.CopKTypes(weakTypeOf[BazBarFooKL], typeOf[Int]))
}

//#-jvm
