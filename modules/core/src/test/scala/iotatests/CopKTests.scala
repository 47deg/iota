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

package iotatests

import iota._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Shapeless._

object CopKTests extends Properties("CopKTests") {

  import KList.::

  sealed abstract class One[A]
  object One {
    case class Value(a: String) extends One[Double]
  }

  sealed abstract class Two[A]
  object Two {
    case class Value(a: Int) extends Two[Int]
  }

  sealed abstract class Three[A]
  object Three {
    case class Value(a: Int) extends Three[Int]
  }

  type OneTwoThree = One :: Two :: Three :: KNil
  type ThreeTwoOne = Three :: Two :: One :: KNil

  def checkInjectL[F[_], L <: KList, A](
    gen: Gen[F[A]],
    inj: CopK.InjectL[F, L],
    index: Int
  ): Prop =
    forAll(gen)(v =>
      inj.inj(v) ?= CopK(index, v))

  property("inject One into OneTwoThree") =
    checkInjectL(
      arbitrary[One.Value],
      CopK.InjectL[One, OneTwoThree],
      0)

  property("inject Two into OneTwoThree") =
    checkInjectL(
      arbitrary[Two.Value],
      CopK.InjectL[Two, OneTwoThree],
      1)

  property("inject Three into OneTwoThree") =
    checkInjectL(
      arbitrary[Three.Value],
      CopK.InjectL[Three, OneTwoThree],
      2)

  property("inject One into ThreeTwoOne") =
    checkInjectL(
      arbitrary[One.Value],
      CopK.InjectL[One, ThreeTwoOne],
      2)

  property("inject Two into ThreeTwoOne") =
    checkInjectL(
      arbitrary[Two.Value],
      CopK.InjectL[Two, ThreeTwoOne],
      1)

  property("inject Three into ThreeTwoOne") =
    checkInjectL(
      arbitrary[Three.Value],
      CopK.InjectL[Three, ThreeTwoOne],
      0)

  type First[A] = A
  type Last[A] = A
  type Y[A]

  type Yuge =
    First :: // 20 rows of 15 = 300 filler items
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Last ::
    KNil


  property("inject First into Yuge") =
    checkInjectL(
      arbitrary[First[Int]],
      CopK.InjectL[First, Yuge],
      0)

  property("inject Last into Yuge") =
    checkInjectL(
      arbitrary[Last[Int]],
      CopK.InjectL[Last, Yuge],
      301)
}
