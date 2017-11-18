iota documentation for Scalaz

## Coproducts

```scala
import iotaz._
import TList.::
import TListK.:::

// a coproduct of types
type Foo = Cop[Int :: String :: Double :: TNil]

// a coproduct of type constructors
type Bar[A] = CopK[Option ::: List ::: Seq ::: TNilK, A]
```

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

One thing to note: `NaturalTransformation` in Scalaz is defined using
variance which doesn't work well with macros that need to infer types.
Consequently some iota methods (`of` and `summon`) require that type
parameters are specified.




```scala
sealed abstract class UserOp[A]
sealed abstract class OrderOp[A]
sealed abstract class PriceOp[A]

type Algebra[A] = CopK[UserOp ::: OrderOp ::: PriceOp ::: TNilK, A]

val evalUserOp : UserOp  ~> IO = dummyInterpreter
val evalOrderOp: OrderOp ~> IO = dummyInterpreter
val evalPriceOp: PriceOp ~> IO = dummyInterpreter

// create the interpreter

val evalAlgebra0: Algebra ~> IO = CopK.NaturalTransformation.of[Algebra, IO](
  evalUserOp, evalOrderOp, evalPriceOp)

// note: order doesn't matter when creating the interpreter since
// iota will sort it out for you

val evalAlgebra1: Algebra ~> IO = CopK.NaturalTransformation.of[Algebra, IO](
  evalOrderOp, evalPriceOp, evalUserOp)

// if your interpreters are implicitly available, you can summon
// a fan in interpreter

implicit val _evalUserOp  = evalUserOp
implicit val _evalOrderOp = evalOrderOp
implicit val _evalPriceOp = evalPriceOp

val evalAlgebra2: Algebra ~> IO = CopK.NaturalTransformation.summon[Algebra, IO]
```

The interpreters created by Iota are optimized for speed and have a
constant evaluation time. Behind the scenes, a macro generates an
integer based switch statement on the coproduct's internal index value.

If you'd like to see the generated code, toggle the "show trees" option by
importing `iota.debug.options.ShowTrees` into scope.

```scala
import iotaz.debug.options.ShowTrees
// import iotaz.debug.options.ShowTrees

CopK.NaturalTransformation.of[Algebra, IO](evalOrderOp, evalPriceOp, evalUserOp)
// <console>:32: {
//   class CopKNaturalTransformation$macro$4 extends _root_.iotaz.internal.FastNaturalTransformation[Algebra, IO] {
//     private[this] val arr0 = evalUserOp.asInstanceOf[_root_.scalaz.NaturalTransformation[Any, scalaz.effect.IO]];
//     private[this] val arr1 = evalOrderOp.asInstanceOf[_root_.scalaz.NaturalTransformation[Any, scalaz.effect.IO]];
//     private[this] val arr2 = evalPriceOp.asInstanceOf[_root_.scalaz.NaturalTransformation[Any, scalaz.effect.IO]];
//     override def apply[Ξ$](η$: Algebra[Ξ$]): IO[Ξ$] = (η$.index: @_root_.scala.annotation.switch) match {
//       case 0 => arr0(η$.value)
//       case 1 => arr1(η$.value)
//       case 2 => arr2(η$.value)
//       case (i @ _) => throw new _root_.java.lang.Exception(StringContext("iota internal error: index ").s().+(i).+(" ...
// res28: iotaz.internal.FastNaturalTransformation[Algebra,scalaz.effect.IO] = FastFunctionK[Algebra, scalaz.effect.IO]<<generated>>
```
