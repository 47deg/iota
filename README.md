# Iota

[comment]: # (Start Badges)

[![Build Status](https://api.travis-ci.org/47deg/iota.png?branch=master)](https://travis-ci.org/47deg/iota)

[comment]: # (End Badges)

## Introduction

Iota is a tiny framework for fast product and coproduct types.

Unlike many coproduct implementations that use a linked list at the
value level, Iota stores indexes for coproduct values to allow for
quick access.

At the type level, Iota uses a linked list type with a binary type
operator for a simple syntax.

```scala
import iota._
import TList.::
import KList.:::

// a coproduct of types
type Foo = Cop[Int :: String :: Double :: TNil]

// a coproduct of type constructors
type Bar[A] = CopK[Option ::: List ::: Seq ::: KNil, A]
```

## Installation

To get started with SBT, simply add the following to your build.sbt file:

[comment]: # (Start Replace)

```scala
libraryDependencies += "com.47deg" %% "iota" % "0.0.1"
```

[comment]: # (End Replace)

## Injection type classes

Iota provides injection type classes to make it easy to get values in
and out of your coproducts.

*coproduct of types*

```scala
val IntFoo    = Cop.Inject[Int,    Foo]
val StringFoo = Cop.Inject[String, Foo]
val DoubleFoo = Cop.Inject[Double, Foo]

def processFoo(foo: Foo): String = foo match {
  case IntFoo(int)       => s"int: $int"
  case StringFoo(string) => s"string: $string"
  case DoubleFoo(double) => s"double: $double"
}

val foo0: Foo = IntFoo.inj(100)
val foo1: Foo = StringFoo.inj("hello world")
val foo2: Foo = DoubleFoo.inj(47.6062)
```
```scala
processFoo(foo0)
// res6: String = int: 100

processFoo(foo1)
// res7: String = string: hello world

processFoo(foo2)
// res8: String = double: 47.6062
```

*coproduct of type constructors*

```scala
val OptionBar = CopK.Inject[Option, Bar]
val ListBar   = CopK.Inject[List,   Bar]
val SeqBar    = CopK.Inject[Seq,    Bar]

def processBar[A](bar: Bar[A]): String = bar match {
  case OptionBar(option) => s"option: $option"
  case ListBar(list)     => s"list: $list"
  case SeqBar(seq)       => s"seq: $seq"
}

val bar0: Bar[Int]    = OptionBar.inj(Some(200))
val bar1: Bar[String] = ListBar.inj("hello" :: "world" :: Nil)
val bar2: Bar[String] = SeqBar.inj(Seq("a", "b", "c"))
```
```scala
processBar(bar0)
// res11: String = option: Some(200)

processBar(bar1)
// res12: String = list: List(hello, world)

processBar(bar2)
// res13: String = seq: List(a, b, c)
```

## Fast Interpreters

If you have interpreters for individual algebras, it's easy to use
Iota create a fast fan in interpreter for the coproduct of your
algebras.

You can ask Iota to create a fan in interpreter by explicitly
passing in individual interpreters for your algebras. Alternatively,
Iota can implicitly summon the requisite interpreters based off the
type signature of your desired interpreter.


```scala
sealed abstract class UserOp[A]
sealed abstract class OrderOp[A]
sealed abstract class PriceOp[A]

type Algebra[A] = CopK[UserOp ::: OrderOp ::: PriceOp ::: KNil, A]

val evalUserOp : UserOp  ~> Future = dummyInterpreter
val evalOrderOp: OrderOp ~> Future = dummyInterpreter
val evalPriceOp: PriceOp ~> Future = dummyInterpreter

// create the interpreter

val evalAlgebra0: Algebra ~> Future = CopK.FunctionK.of(
  evalUserOp, evalOrderOp, evalPriceOp)

// note: order doesn't matter when creating the interpreter since
// iota will sort it out for you

val evalAlgebra1: Algebra ~> Future = CopK.FunctionK.of(
  evalOrderOp, evalPriceOp, evalUserOp)

// if your interpreters are implicitly available, you can summon
// a fan in interpreter

implicit val _evalUserOp  = evalUserOp
implicit val _evalOrderOp = evalOrderOp
implicit val _evalPriceOp = evalPriceOp

val evalAlgebra2: Algebra ~> Future = CopK.FunctionK.summon
```

The interpreters created by Iota are optimized for speed and have a
constant evalaluation time. Behind the scenes, a macro generates an
integer based switch statement on the coproduct's internal index value.

If you'd like to see the generated code, toggle the "show trees" option by
importing `iota.debug.options.ShowTrees` into scope.

```scala
import iota.debug.options.ShowTrees
// import iota.debug.options.ShowTrees

CopK.FunctionK.of[Algebra, Future](evalOrderOp, evalPriceOp, evalUserOp)
// <console>:30: generated tree:
// {
//   final class $anon extends iota.CopKFunctionK[Algebra, Future] {
//     private[this] def arr0 = evalUserOp.asInstanceOf[cats.arrow.FunctionK[Any, Future]];
//     private[this] def arr1 = evalOrderOp.asInstanceOf[cats.arrow.FunctionK[Any, Future]];
//     private[this] def arr2 = evalPriceOp.asInstanceOf[cats.arrow.FunctionK[Any, Future]];
//     override def apply[A](ca: Algebra[A]): Future[A] = (ca.index: @scala.annotation.switch) match {
//       case 0 => arr0(ca.value)
//       case 1 => arr1(ca.value)
//       case 2 => arr2(ca.value)
//       case (i @ _) => throw new java.lang.Exception("internal iota error")
//     };
//     override def toString: String = "CopKFunctionK[Algebra, Future]<<generated>>"
//   };
//   new $anon()
// }
//        CopK.FunctionK.of[Algebra, Future](ev...
// res28: iota.CopKFunctionK[Algebra,scala.concurrent.Future] = CopKFunctionK[Algebra, Future]<<generated>>
```

## Free

A `Free` example is available [in the tests][free example].

## Iota in the wild

If you wish to add your library here please consider a PR to include it in the list below.

[comment]: # (Start Copyright)
# Copyright

Iota is designed and developed by 47 Degrees

Copyright (C) 2016-2017 47 Degrees. <http://47deg.com>

[comment]: # (End Copyright)