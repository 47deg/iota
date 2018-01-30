package example

import iota.TNilK
import iota.TListK.::
import iota.syntax.evidence._

object Evidence extends App {

  class Foo[A]
  class Bar[A]
  class Zed[A]

  // firstK will find the first available evidence for entries in a TListK

  {
    implicit val zedForString: Zed[String] = new Zed[String]
    assert(firstK[Foo :: Bar :: Zed :: TNilK, String].value == zedForString)

    {
      implicit val barForString: Bar[String] = new Bar[String]
      assert(firstK[Foo :: Bar :: Zed :: TNilK, String].value == barForString)
      assert(firstK[Foo :: Zed :: Bar :: TNilK, String].value == zedForString)

      {
        implicit val fooForString: Foo[String] = new Foo[String]
        assert(firstK[Foo :: Bar :: Zed :: TNilK, String].value == fooForString)
        assert(firstK[Bar :: Foo :: Zed :: TNilK, String].value == barForString)
        assert(firstK[Zed :: Bar :: Foo :: TNilK, String].value == zedForString)
      }
    }
  }

}
