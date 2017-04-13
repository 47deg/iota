/* -
 * Iota [iota-core]
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

  implicit val evalAddOne: AddOne ~> Id = λ[AddOne ~> Id] { case AddOne.Value(v) => v + 1 }
  implicit val evalXTwo  : XTwo   ~> Id = λ[XTwo   ~> Id] { case XTwo.Value  (v) => v * 2 }
  implicit val evalNeg   : Neg    ~> Id = λ[Neg    ~> Id] { case Neg.Value   (v) => -v    }
  implicit val evalHalf  : Half   ~> Id = λ[Half   ~> Id] { case Half.Value  (v) => v / 2 }

  val eval = CopKFunctionK.summon[Algebra, Id]

  import CopK.liftFree

  val program: Free[Algebra, Int] =
    for {
      `101`  <- liftFree[Algebra](AddOne.Value(100))
      `-101` <- liftFree[Algebra](Neg.Value(`101`))
      `-202` <- liftFree[Algebra](XTwo.Value(`-101`))
    } yield `-101` + `-202`

  property("basic math program") =
    program.foldMap(eval) ?= -303

}
