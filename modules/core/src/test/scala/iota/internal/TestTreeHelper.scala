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

import catryoshka._

class TestTreeHelper(val tb: Toolbelt with TypeListAST) {
  import tb._
  import tb.u.{ Type, WeakTypeTag }

  def t[T](implicit evT: WeakTypeTag[T]): Type = evT.tpe

  type Node = Fix[NodeF]

  val nnil                          : Fix[NodeF] = Fix[NodeF](NNilF)
  def cons[T](node: Fix[NodeF] = nnil)(
    implicit evT: WeakTypeTag[T])   : Fix[NodeF] = Fix[NodeF](ConsF(evT.tpe, node))
  def consk[T[_]](node: Fix[NodeF] = nnil)(
    implicit evT: WeakTypeTag[T[_]]): Fix[NodeF] = Fix[NodeF](ConsF(evT.tpe.typeConstructor, node))
  def concat(nodes: Fix[NodeF]*)    : Fix[NodeF] = Fix[NodeF](ConcatF(nodes.toList))
  def reverse(node: Fix[NodeF])     : Fix[NodeF] = Fix[NodeF](ReverseF(node))
  def take(n: Int, node: Fix[NodeF]): Fix[NodeF] = Fix[NodeF](TakeF(n, node))
  def drop(n: Int, node: Fix[NodeF]): Fix[NodeF] = Fix[NodeF](DropF(n, node))
  def remove[T](node: Fix[NodeF] = nnil)(
    implicit evT: WeakTypeTag[T])   : Fix[NodeF] = Fix[NodeF](RemoveF(evT.tpe, node))
  def removek[T[_]](node: Fix[NodeF] = nnil)(
    implicit evT: WeakTypeTag[T[_]]): Fix[NodeF] = Fix[NodeF](RemoveF(evT.tpe.typeConstructor, node))
}

//#-jvm
