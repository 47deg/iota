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

package iota_bench

import iota._

import cats._
import cats.arrow.FunctionK
import cats.data.{State => _, _}
import cats.free._

import org.scalacheck._
//import org.scalacheck.Gen.oneOf

sealed abstract class InjK[F[_], G[_]] {
  def inj: FunctionK[F, G]
  def prj: FunctionK[G, λ[α => Option[F[α]]]]
  final def apply[A](fa: F[A]): G[A] = inj(fa)
  final def unapply[A](ga: G[A]): Option[F[A]] = prj(ga)
}

object InjK {
  implicit def injKFromCatsInject[F[_], G[_]](
      implicit ev: Inject[F, G]
  ): InjK[F, G] = new InjK[F, G] {
    def inj = λ[F ~> G](ev.inj(_))
    def prj = λ[G ~> λ[α => Option[F[α]]]](ev.prj(_))
  }

  implicit def injKfromCopKInj[F[_], L <: KList](
      implicit ev: CopK.InjectL[F, L]
  ): InjK[F, CopK[L, ?]] = new InjK[F, CopK[L, ?]] {
    def inj = λ[F ~> CopK[L, ?]](ev.inj(_))
    def prj = λ[CopK[L, ?] ~> λ[α => Option[F[α]]]](ev.proj(_))
  }
}

object Cats {
  import Ops._

  type AlgebraA[A] = AOp[A]
  type AlgebraB[A] = Coproduct[BOp, AlgebraA, A]
  type AlgebraC[A] = Coproduct[COp, AlgebraB, A]
  type AlgebraD[A] = Coproduct[DOp, AlgebraC, A]
  type AlgebraE[A] = Coproduct[EOp, AlgebraD, A]

  val evalA: AlgebraA ~> Id = AOp.eval
  val evalB: AlgebraB ~> Id = BOp.eval or evalA
  val evalC: AlgebraC ~> Id = COp.eval or evalB
  val evalD: AlgebraD ~> Id = DOp.eval or evalC
  val evalE: AlgebraE ~> Id = EOp.eval or evalD

  private[this] implicit class InjGenOps[F[_]](gf: Gen[F[_]]) {
    def inj[G[_]](implicit ev: InjK[F, G]): Gen[G[_]] = gf.map(f => ev(f))
  }

  /*
  val genAlgebraA: Gen[AlgebraA[_]] = AOp.gen.inj[AlgebraA]
  val genAlgebraB: Gen[AlgebraB[_]] = oneOf(AOp.gen.inj[AlgebraB], BOp.gen.inj[AlgebraB])
  val genAlgebraC: Gen[AlgebraC[_]] = oneOf(AOp.gen.inj[AlgebraC], BOp.gen.inj[AlgebraC], COp.gen.inj[AlgebraC])
  val genAlgebraD: Gen[AlgebraD[_]] = oneOf(AOp.gen.inj[AlgebraD], BOp.gen.inj[AlgebraD], COp.gen.inj[AlgebraD], COp.gen.inj[AlgebraD])
  val genAlgebraE: Gen[AlgebraE[_]] = oneOf(AOp.gen.inj[AlgebraE], BOp.gen.inj[AlgebraE], COp.gen.inj[AlgebraE], COp.gen.inj[AlgebraE], DOp.gen.inj[AlgebraE])*/

  val genAlgebraA: Gen[AlgebraA[_]] = AOp.gen.inj[AlgebraA]
  val genAlgebraB: Gen[AlgebraB[_]] = AOp.gen.inj[AlgebraB]
  val genAlgebraC: Gen[AlgebraC[_]] = AOp.gen.inj[AlgebraC]
  val genAlgebraD: Gen[AlgebraD[_]] = AOp.gen.inj[AlgebraD]
  val genAlgebraE: Gen[AlgebraE[_]] = AOp.gen.inj[AlgebraE]
}

object Iota {
  import Ops._

  import KList.::
  type AlgebraA[A] = CopK[AOp :: KNil, A]
  type AlgebraB[A] = CopK[BOp :: AOp :: KNil, A]
  type AlgebraC[A] = CopK[COp :: BOp :: AOp :: KNil, A]
  type AlgebraD[A] = CopK[DOp :: COp :: BOp :: AOp :: KNil, A]
  type AlgebraE[A] = CopK[EOp :: DOp :: COp :: BOp :: AOp :: KNil, A]

  val evalA: AlgebraA ~> Id = CopK.FunctionK.of(AOp.eval)
  val evalB: AlgebraB ~> Id = CopK.FunctionK.of(AOp.eval, BOp.eval)
  val evalC: AlgebraC ~> Id = CopK.FunctionK.of(AOp.eval, BOp.eval, COp.eval)
  val evalD: AlgebraD ~> Id =
    CopK.FunctionK.of(AOp.eval, BOp.eval, COp.eval, DOp.eval)
  val evalE: AlgebraE ~> Id =
    CopK.FunctionK.of(AOp.eval, BOp.eval, COp.eval, DOp.eval, EOp.eval)

  private[this] implicit class InjGenOps[F[_]](gf: Gen[F[_]]) {
    def inj[G[_]](implicit ev: InjK[F, G]): Gen[G[_]] = gf.map(f => ev(f))
  }

  /*
  val genAlgebraA: Gen[AlgebraA[_]] = AOp.gen.inj[AlgebraA]
  val genAlgebraB: Gen[AlgebraB[_]] = oneOf(AOp.gen.inj[AlgebraB], BOp.gen.inj[AlgebraB])
  val genAlgebraC: Gen[AlgebraC[_]] = oneOf(AOp.gen.inj[AlgebraC], BOp.gen.inj[AlgebraC], COp.gen.inj[AlgebraC])
  val genAlgebraD: Gen[AlgebraD[_]] = oneOf(AOp.gen.inj[AlgebraD], BOp.gen.inj[AlgebraD], COp.gen.inj[AlgebraD], COp.gen.inj[AlgebraD])
  val genAlgebraE: Gen[AlgebraE[_]] = oneOf(AOp.gen.inj[AlgebraE], BOp.gen.inj[AlgebraE], COp.gen.inj[AlgebraE], COp.gen.inj[AlgebraE], DOp.gen.inj[AlgebraE])
   */

  val genAlgebraA: Gen[AlgebraA[_]] = AOp.gen.inj[AlgebraA]
  val genAlgebraB: Gen[AlgebraB[_]] = AOp.gen.inj[AlgebraB]
  val genAlgebraC: Gen[AlgebraC[_]] = AOp.gen.inj[AlgebraC]
  val genAlgebraD: Gen[AlgebraD[_]] = AOp.gen.inj[AlgebraD]
  val genAlgebraE: Gen[AlgebraE[_]] = AOp.gen.inj[AlgebraE]
}

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class Bench {

  /*
  @Benchmark def catsGenA: Any = Cats.genAlgebraA.sample.get
  @Benchmark def catsGenB: Any = Cats.genAlgebraB.sample.get
  @Benchmark def catsGenC: Any = Cats.genAlgebraC.sample.get
  @Benchmark def catsGenD: Any = Cats.genAlgebraD.sample.get
  @Benchmark def catsGenE: Any = Cats.genAlgebraE.sample.get

  @Benchmark def iotaGenA: Any = Iota.genAlgebraA.sample.get
  @Benchmark def iotaGenB: Any = Iota.genAlgebraB.sample.get
  @Benchmark def iotaGenC: Any = Iota.genAlgebraC.sample.get
  @Benchmark def iotaGenD: Any = Iota.genAlgebraD.sample.get
  @Benchmark def iotaGenE: Any = Iota.genAlgebraE.sample.get

  @Benchmark def catsEvalA: Id[_] = Cats.evalA(Cats.genAlgebraA.sample.get)
  @Benchmark def catsEvalB: Id[_] = Cats.evalB(Cats.genAlgebraB.sample.get)
  @Benchmark def catsEvalC: Id[_] = Cats.evalC(Cats.genAlgebraC.sample.get)
  @Benchmark def catsEvalD: Id[_] = Cats.evalD(Cats.genAlgebraD.sample.get)
  @Benchmark def catsEvalE: Id[_] = Cats.evalE(Cats.genAlgebraE.sample.get)
   */

  @Benchmark def iotaEvalA: Id[_] = Iota.evalA(Iota.genAlgebraA.sample.get)
  @Benchmark def iotaEvalB: Id[_] = Iota.evalB(Iota.genAlgebraB.sample.get)
  @Benchmark def iotaEvalC: Id[_] = Iota.evalC(Iota.genAlgebraC.sample.get)
  @Benchmark def iotaEvalD: Id[_] = Iota.evalD(Iota.genAlgebraD.sample.get)
  @Benchmark def iotaEvalE: Id[_] = Iota.evalE(Iota.genAlgebraE.sample.get)

}

/*
object BenchCodeGen extends App {
  import scala.Predef._

  def catsAlgebraHeadTemplate(
    name: String
  ): String =
    s"type Algebra$name[A] = ${name}Op[A]"

  def catsAlgebraTailTemplate(
    prevName: String,
    name: String
  ): String =
    s"type Algebra$name[A] = Coproduct[${name}Op, Algebra$prevName, A]"

  def catsEvalHeadTemplate(
    name: String
  ): String =
    s"val eval${name}: Algebra${name} ~> Id = ${name}Op.eval"

  def catsEvalTailTemplate(
    prevName: String,
    name: String
  ): String =
    s"val eval$name: AlgebraB ~> Id = BOp.eval or evalA"

  def opsTemplate(
    name: String
  ): String = s"""
    |  sealed trait ${name}Op[A]
    |  object ${name}Op {
    |    case class Op1(v: Int) extends ${name}Op[Int]
    |    lazy val eval = λ[${name}Op ~> Id] { case ${name}Op.Op1(v) => v + 1 }
    |  }
    |  class ${name}Ops[F[_]](implicit inj: InjK[${name}Op, F]) {
    |    def op1(v: Int): Free[F, Int] = Free.liftF(inj(${name}Op.Op1(v)))
    |  }""".stripMargin

  val alphabet = List(
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
    "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")

  alphabet.take(5).foreach { l =>
    //println(template(l.toUpperCase))
  }

}
 */

package Ops {

  sealed trait AOp[A]
  object AOp {
    case class Op1(v: Int) extends AOp[Int]
    val eval = λ[AOp ~> Id] { case AOp.Op1(v) => v + 1 }
    val gen: Gen[AOp[_]] = Gen.const(Op1(100)) // arbitrary[Int].map(Op1)
  }
  class AOps[F[_]](implicit inj: InjK[AOp, F]) {
    def op1(v: Int): Free[F, Int] = Free.liftF(inj(AOp.Op1(v)))
  }

  sealed trait BOp[A]
  object BOp {
    case class Op1(v: Int) extends BOp[Int]
    val eval = λ[BOp ~> Id] { case BOp.Op1(v) => v + 1 }
    val gen: Gen[BOp[_]] = Gen.const(Op1(100))
  }
  class BOps[F[_]](implicit inj: InjK[BOp, F]) {
    def op1(v: Int): Free[F, Int] = Free.liftF(inj(BOp.Op1(v)))
  }

  sealed trait COp[A]
  object COp {
    case class Op1(v: Int) extends COp[Int]
    val eval = λ[COp ~> Id] { case COp.Op1(v) => v + 1 }
    val gen: Gen[COp[_]] = Gen.const(Op1(100))
  }
  class COps[F[_]](implicit inj: InjK[COp, F]) {
    def op1(v: Int): Free[F, Int] = Free.liftF(inj(COp.Op1(v)))
  }

  sealed trait DOp[A]
  object DOp {
    case class Op1(v: Int) extends DOp[Int]
    val eval = λ[DOp ~> Id] { case DOp.Op1(v) => v + 1 }
    val gen: Gen[DOp[_]] = Gen.const(Op1(100))
  }
  class DOps[F[_]](implicit inj: InjK[DOp, F]) {
    def op1(v: Int): Free[F, Int] = Free.liftF(inj(DOp.Op1(v)))
  }

  sealed trait EOp[A]
  object EOp {
    case class Op1(v: Int) extends EOp[Int]
    val eval = λ[EOp ~> Id] { case EOp.Op1(v) => v + 1 }
    val gen: Gen[EOp[_]] = Gen.const(Op1(100))
  }
  class EOps[F[_]](implicit inj: InjK[EOp, F]) {
    def op1(v: Int): Free[F, Int] = Free.liftF(inj(EOp.Op1(v)))
  }
}
