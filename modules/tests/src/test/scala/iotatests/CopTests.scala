package iotatests

import iota._  //#=cats
import iotaz._ //#=scalaz

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.scalacheck.ScalacheckShapeless._

object CopTests extends Properties("CopTests") {

  import TList.::
  import TList.Op._

  case class One(a: String)
  case class Two(a: Int)
  case class Three(a: Int)

  type OneTwoThreeL = One :: Two :: Three :: TNil
  type ThreeTwoOneL = Three :: Two :: One :: TNil

  // these just need to compile
  Cop.InjectL[One, OneTwoThreeL]
  Cop.InjectL[CopTests.One, OneTwoThreeL]
  Cop.InjectL[_root_.iotatests.CopTests.One, OneTwoThreeL]

  def checkInjectL[A, L <: TList](
    gen: Gen[A],
    inj: Cop.InjectL[A, L],
    index: Int
  ): Prop =
    forAll(gen)(v =>
      inj.inj(v) ?= Cop.unsafeApply(index, v))

  property("inject One into OneTwoThreeL") =
    checkInjectL(
      arbitrary[One],
      Cop.InjectL[One, OneTwoThreeL],
      0)

  property("inject Two into OneTwoThreeL") =
    checkInjectL(
      arbitrary[Two],
      Cop.InjectL[Two, OneTwoThreeL],
      1)

  property("inject Three into OneTwoThreeL") =
    checkInjectL(
      arbitrary[Three],
      Cop.InjectL[Three, OneTwoThreeL],
      2)

  property("inject One into ThreeTwoOneL") =
    checkInjectL(
      arbitrary[One],
      Cop.InjectL[One, ThreeTwoOneL],
      2)

  property("inject Two into ThreeTwoOneL") =
    checkInjectL(
      arbitrary[Two],
      Cop.InjectL[Two, ThreeTwoOneL],
      1)

  property("inject Three into ThreeTwoOneL") =
    checkInjectL(
      arbitrary[Three],
      Cop.InjectL[Three, ThreeTwoOneL],
      0)

  property("inject Three into Reverse[ThreeTwoOneL]") =
    checkInjectL(
      arbitrary[Three],
      Cop.InjectL[Three, Reverse[ThreeTwoOneL]],
      2)

  //#+cats
  type First = Int
  type Last  = String
  type Y

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
    TNil


  property("inject First into Yuge") =
    checkInjectL(
      arbitrary[First],
      Cop.InjectL[First, Yuge],
      0)

  property("inject Last into Yuge") =
    checkInjectL(
      arbitrary[Last],
      Cop.InjectL[Last, Yuge],
      301)
  //#-cats
}
