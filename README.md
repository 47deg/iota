
[comment]: # (Start Badges)

[![Build Status](https://travis-ci.org/frees-io/iota.svg?branch=master)](https://travis-ci.org/frees-io/iota) [![Maven Central](https://img.shields.io/badge/maven%20central-0.3.2-green.svg)](https://oss.sonatype.org/#nexus-search;gav~io.frees~iota*) [![License](https://img.shields.io/badge/license-Apache%202-blue.svg)](https://raw.githubusercontent.com/frees-io/iota/master/LICENSE) [![Latest version](https://img.shields.io/badge/iota-0.3.2-green.svg)](https://index.scala-lang.org/frees-io/iota) [![Scala.js](http://scala-js.org/assets/badges/scalajs-0.6.20.svg)](http://scala-js.org) [![GitHub Issues](https://img.shields.io/github/issues/frees-io/iota.svg)](https://github.com/frees-io/iota/issues)

[comment]: # (End Badges)

# Iota

## Introduction

Iota is a tiny library for fast coproduct types with a syntax
that cleanly supports the disjunction of any number of types.

Traditional coproduct implementations are implemented as binary trees
or linked lists at both the type and value level. The syntax for
traditional coproducts frequently becomes unwieldy as the number of
disjunct types grows.

```scala
// a coproduct of types using scala.util.Either
type EitherFoo = Either[Int, Either[String, Double]]

// a coproduct of type constructors using cats.data.EitherK
import cats.data.EitherK
type EitherKBar0[A] = EitherK[List, Seq, A]
type EitherKBar[A]  = EitherK[Option, EitherKBar0, A]

// a coproduct of type constructors using scalaz.Coproduct
import scalaz.Coproduct
type CoproductKBar0[A] = Coproduct[List, Seq, A]
type CoproductKBar[A]  = Coproduct[Option, CoproductKBar0, A]
```

Iota coproducts are linked lists at the type level. At the value level,
Iota stores the index of the disjunct value's type for quick and
constant time access of the values. This syntax scales cleanly to
support any number of disjunct types.

```scala
// for cats
import iota._
import TList.::
import TListK.:::

// a coproduct of types
type Foo = Cop[Int :: String :: Double :: TNil]

// a coproduct of type constructors
type Bar[A] = CopK[Option ::: List ::: Seq ::: TNilK, A]
```

```scala
// for scalaz
import iotaz._
import TList.::
import TListK.:::

// a coproduct of types
type Foo = Cop[Int :: String :: Double :: TNil]

// a coproduct of type constructors
type Bar[A] = CopK[Option ::: List ::: Seq ::: TNilK, A]
```

## Installation

To get started with SBT, simply add the following to your build.sbt file.

For Scala 2.11.x and 2.12.x:

[comment]: # (Start Replace)

```scala
libraryDependencies += "io.frees" %% "iota-core"  % "0.3.2" // for cats
libraryDependencies += "io.frees" %% "iotaz-core" % "0.3.2" // for scalaz
```

Or, if using Scala.js (0.6.x):

```scala
libraryDependencies += "io.frees" %%% "iota-core"  % "0.3.2" // for cats
libraryDependencies += "io.frees" %%% "iotaz-core" % "0.3.2" // for scalaz
```

[comment]: # (End Replace)

## Cats vs Scalaz

Iota requires either Cats or Scalaz. If you're using Scalaz, use the "iotaz"
modules and import from the `iotaz` package (instead of `iota`).

Cats friendly terminology (such as "FunctionK") is used in the iota
modules while Scalaz friendly terminology (such as
"NaturalTransformation") is used in the iotaz modules. If you find an
issue or inconsistency, please file a GitHub issue and it will be fixed.

The Cats examples will work against Scalaz, and vise versa, so long as the
library specific terminology is adjusted. Expect more Scalaz examples as the
Iota library evolves.

## Documentation
See [docs/cats.md](docs/cats.md) for the Cats specific documentation and
[docs/scalaz.md](docs/scalaz.md) for the Scalaz specific documentation.

## Iota in the wild

If you wish to add your library here please consider a PR to include it in the list below.

## Commercial Support

47 Degrees offers commercial support for the iota library and associated technologies. To find out more, visit [47 Degrees' Open Source Support](https://www.47deg.com/services/open-source-support/).

[comment]: # (Start Copyright)
# Copyright

Iota is designed and developed by 47 Degrees

Copyright (C) 2016-2017 47 Degrees. <http://47deg.com>

[comment]: # (End Copyright)

[free example]: modules/tests/src/test/scala/iotatests/FreeCopKTests.scala
