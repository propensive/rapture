/*
  Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.

  The primary distribution site is
  
    http://rapture.io/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  
    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the License is
  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and limitations under the License.
*/

package rapture.cli

import language.implicitConversions
import language.higherKinds

object Tabulation {
  sealed trait Position
  case object Right extends Position
  case object Left extends Position
  
  trait Col_1 {
    implicit def anyToCol(s: Any): Col = Col(s.toString, Left)
  }
  object Col extends Col_1 {
    implicit def intToCol(s: Int): Col = Col(s.toString, Right)
    implicit def stringToCol(s: String): Col = Col(s, Left)
  }
  case class Col(content: String, position: Position = Left)
  def tabulate[C[E] <: Seq[E], T](collection: Seq[T], titles: Option[Seq[Col]] = None)(cols:
      (T => Col)*): Seq[String] = {
    val contents = collection.map { e => cols.map(_(e)) }
    
    val contentsWithTitles = titles.map { ts =>
      ts +: (ts.map { case Col(s, p) => Col(s.map(c => '-'), p) }) +: contents
    }.getOrElse(contents)
    
    val widths = contentsWithTitles.map { _.map(_.content.length) }.reduce(_ zip _ map { case (a, b) => a max b })

    contentsWithTitles.map(_.zip(widths).map {
      case (Col(s, Right), w) => " "*(w - s.length)+s+"  "
      case (Col(s, Left), w) => s+(" "*(w - s.length))+"  "
    }.mkString)
  }
}
