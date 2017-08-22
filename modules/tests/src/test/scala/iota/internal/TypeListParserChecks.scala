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

package iota
package internal

import scala.Predef.ArrowAssoc

import org.scalacheck._
import org.scalacheck.Prop._

import cats.instances.either._
import catryoshka._

import iotatests.FooAndFriends._

object TypeListParserChecks extends Properties("TypeListParsers") {

  val checks = new TypeListParserChecks(IotaReflectiveToolbelt())

  checks.tlists.foreach { case (in, out) =>
    property(s"parse TList $in") = Corecursive[checks.Node].anaM(in)(checks.tb.tlistParser) ?= Right(out) }

  checks.tlistks.foreach { case (in, out) =>
    property(s"parse TListK $in") = Corecursive[checks.Node].anaM(in)(checks.tb.tlistkParser) ?= Right(out) }

}

class TypeListParserChecks(
  override val tb: Toolbelt with TypeListAST with TypeListParsers
) extends TestTreeHelper(tb) {

  import tb.u.Type

  import TList.::
  import TList.Op.{
    Concat  => TConcat,
    Reverse => TReverse,
    Take    => TTake,
    Drop    => TDrop,
    Remove  => TRemove
  }

  val tlists: List[(Type, Node)] = List(
    t[TNil] -> nnil,
    t[Int :: TNil] -> cons[Int](),
    t[String :: Int :: TNil] -> cons[String](cons[Int]()),

    t[TReverse[TNil]] -> reverse(nnil),

    t[Cop[BazBarFooL]#L] -> cons[Baz](cons[Bar](cons[Foo]())),

    t[TConcat[BazBarFoo#L, FooBarBaz#L]] -> concat(
      cons[Baz](cons[Bar](cons[Foo]())),
      cons[Foo](cons[Bar](cons[Baz]()))
    ),

    t[TTake[0, BazBarFoo#L]] -> take(0, cons[Baz](cons[Bar](cons[Foo]()))),
    t[TTake[1, BazBarFoo#L]] -> take(1, cons[Baz](cons[Bar](cons[Foo]()))),

    t[TDrop[2, BazBarFoo#L]] -> drop(2, cons[Baz](cons[Bar](cons[Foo]()))),
    t[TDrop[3, BazBarFoo#L]] -> drop(3, cons[Baz](cons[Bar](cons[Foo]()))),

    t[TRemove[Baz, BazBarFoo#L]] -> remove[Baz](cons[Baz](cons[Bar](cons[Foo]()))),
    t[TRemove[Foo, BazBarFoo#L]] -> remove[Foo](cons[Baz](cons[Bar](cons[Foo]())))
  )

  import TListK.Op.{
    Concat  => KConcat,
    Reverse => KReverse,
    Take    => KTake,
    Drop    => KDrop,
    Remove  => KRemove
  }

  val tlistks: List[(Type, Node)] = List(

    t[KReverse[TNilK]] -> reverse(nnil),

    t[CopK[BazBarFooKL, Nothing]#L] -> consk[BazK](consk[BarK](consk[FooK]())),

    t[KConcat[BazBarFooK[_]#L, FooBarBazK[_]#L]] -> concat(
      consk[BazK](consk[BarK](consk[FooK]())),
      consk[FooK](consk[BarK](consk[BazK]()))
    ),

    t[KTake[0, BazBarFooK[_]#L]] -> take(0, consk[BazK](consk[BarK](consk[FooK]()))),
    t[KTake[1, BazBarFooK[_]#L]] -> take(1, consk[BazK](consk[BarK](consk[FooK]()))),

    t[KDrop[2, BazBarFooK[_]#L]] -> drop(2, consk[BazK](consk[BarK](consk[FooK]()))),
    t[KDrop[3, BazBarFooK[_]#L]] -> drop(3, consk[BazK](consk[BarK](consk[FooK]()))),

    t[KRemove[BazK, BazBarFooK[_]#L]] -> removek[BazK](consk[BazK](consk[BarK](consk[FooK]()))),
    t[KRemove[FooK, BazBarFooK[_]#L]] -> removek[FooK](consk[BazK](consk[BarK](consk[FooK]())))
  )

}

//#-jvm
