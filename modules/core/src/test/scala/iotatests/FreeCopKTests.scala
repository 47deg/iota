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

import cats._
import cats.free._

import org.scalacheck._
import org.scalacheck.Prop._

import iota._

object MathAlgebraKs {
  sealed abstract class AddOne[A]
  object AddOne {
    case class Value(a: Int) extends AddOne[Int]
  }

  sealed abstract class XTwo[A]
  object XTwo {
    case class Value(a: Int) extends XTwo[Int]
  }

  sealed abstract class Neg[A]
  object Neg {
    case class Value(a: Int) extends Neg[Int]
  }

  sealed abstract class Half[A]
  object Half {
    case class Value(a: Int) extends Half[Int]
  }
}

object FreeCopK extends Properties("FreeCopK") {

  import KList.::
  import MathAlgebraKs._
  type Algebra[A]  = CopK[AddOne :: XTwo :: Neg :: Half :: KNil, A]

  implicit lazy val evalAddOne: AddOne ~> Id = λ[AddOne ~> Id] { case AddOne.Value(v) => v + 1 }
  implicit lazy val evalXTwo  : XTwo   ~> Id = λ[XTwo   ~> Id] { case XTwo.Value  (v) => v * 2 }
  implicit lazy val evalNeg   : Neg    ~> Id = λ[Neg    ~> Id] { case Neg.Value   (v) => -v    }
  implicit lazy val evalHalf  : Half   ~> Id = λ[Half   ~> Id] { case Half.Value  (v) => v / 2 }

  import CopK.liftFree

  lazy val program: Free[Algebra, Int] =
    for {
      `101`  <- liftFree[Algebra](AddOne.Value(100))
      `-101` <- liftFree[Algebra](Neg.Value(`101`))
      `-202` <- liftFree[Algebra](XTwo.Value(`-101`))
    } yield `-101` + `-202`

  lazy val evalSummon = CopK.FunctionK.summon[Algebra, Id]

  property("basic math program through summoned interpreter") =
    program.foldMap(evalSummon) ?= -303

  lazy val evalOf     = CopK.FunctionK.of[Algebra, Id](
    evalAddOne, evalXTwo, evalHalf, evalNeg)

  property("basic math program regular interpreter") =
    program.foldMap(evalOf) ?= -303

}
