package example

import iota._
import iota.syntax.all._

import scala.Predef._

object InjectSyntax extends App {
  import TList.::
  import TListK.:::

  object Luna
  object Mabel
  object Finn
  object Cinder
  object Bridger

  type Dog = Cop[
    Luna.type :: Mabel.type :: Finn.type :: Cinder.type :: Bridger.type :: TNil]

  val dog0: Dog = Luna.inject[Dog]
  val dog1: Dog = Finn.inject[Dog]

  println(dog0)
  println(dog1)

  type Wat[A] = CopK[List ::: Option ::: TNilK, A]

  val res0: Wat[String] = List("hello", "world").injectK[Wat]
  val res1: Wat[Int]    = Option(1).injectK[Wat]
}
