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

//#+jvm

package iota
package internal

import scala.Predef.ArrowAssoc

import org.scalacheck._
import org.scalacheck.Prop._

import catryoshka._

object TypeListEvaluationChecks extends Properties("TypeListEvaluators") {

  val checks = new TypeListEvaluationChecks(IotaReflectiveToolbelt())

  checks.evalChecks.foreach { case (in, out) =>
    property(s"eval $in") = Recursive[checks.Node].cata(in)(checks.tb.evalTree) ?= out }
}

class TypeListEvaluationChecks(
  override val tb: Toolbelt with TypeListEvaluators with TypeListAST
) extends TestTreeHelper(tb) {

  import tb.u.Type

  val evalChecks: List[(Node, List[Type])] = List(

    nnil               -> (Nil),
    reverse(nnil)      -> (Nil),
    concat(nnil)       -> (Nil),
    concat(nnil, nnil) -> (Nil),

    cons[Int]()        -> (t[Int] :: Nil),
    cons[String]()     -> (t[String] :: Nil),
    cons[Double]()     -> (t[Double] :: Nil),

    cons[Double](cons[String](cons[Int]()))
      -> (t[Double] :: t[String] :: t[Int] :: Nil),

    reverse(cons[Double](cons[String](cons[Int]())))
      -> (t[Int] :: t[String] :: t[Double] :: Nil),

    concat(
      cons[Double](cons[String](cons[Int]())),
      cons[Double](cons[String](cons[Int]()))
    ) -> (t[Double] :: t[String] :: t[Int] :: t[Double] :: t[String] :: t[Int] :: Nil),

    concat(
      cons[Double](cons[String](cons[Int]())),
      nnil,
      cons[Double](cons[String](cons[Int]()))
    ) -> (t[Double] :: t[String] :: t[Int] :: t[Double] :: t[String] :: t[Int] :: Nil),

    concat(
      cons[Double](cons[String](cons[Int]())),
      reverse(cons[Double](cons[String](cons[Int]())))
    ) -> (t[Double] :: t[String] :: t[Int] :: t[Int] :: t[String] :: t[Double] :: Nil),

    take(0, cons[Double]())               -> (Nil),
    take(1, cons[Double]())               -> (t[Double] :: Nil),
    take(2, cons[Double]())               -> (t[Double] :: Nil),
    take(0, cons[String](cons[Double]())) -> (Nil),
    take(1, cons[String](cons[Double]())) -> (t[String] :: Nil),
    take(2, cons[String](cons[Double]())) -> (t[String] :: t[Double] :: Nil),
    take(3, cons[String](cons[Double]())) -> (t[String] :: t[Double] :: Nil),

    drop(0, cons[Double]())               -> (t[Double] :: Nil),
    drop(1, cons[Double]())               -> (Nil),
    drop(2, cons[Double]())               -> (Nil),
    drop(0, cons[String](cons[Double]())) -> (t[String] :: t[Double] :: Nil),
    drop(1, cons[String](cons[Double]())) -> (t[Double] :: Nil),
    drop(2, cons[String](cons[Double]())) -> (Nil),
    drop(3, cons[String](cons[Double]())) -> (Nil),

    remove[Double](cons[Double](cons[String](cons[Int]()))) -> (t[String] :: t[Int] :: Nil),
    remove[String](cons[Double](cons[String](cons[Int]()))) -> (t[Double] :: t[Int] :: Nil),
    remove[Int   ](cons[Double](cons[String](cons[Int]()))) -> (t[Double] :: t[String] :: Nil),
    remove[String](cons[String](cons[String](cons[Int]()))) -> (t[String] :: t[Int] :: Nil)

  )

}

//#-jvm
