package iotatests

import iota._  //#=cats
import iotaz._ //#=scalaz

import test.LiteralInt
import test.LiteralString

object TestSingletonLiterals {

  type `0` = LiteralInt.`0`.T
  type `1` = LiteralInt.`1`.T
  type `2` = LiteralInt.`2`.T
  type `3` = LiteralInt.`3`.T

  type `"foo"` = LiteralString.foo.T
  type `"bar"` = LiteralString.bar.T
  type `"baz"` = LiteralString.baz.T

}
