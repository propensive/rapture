/******************************************************************************************************************\
* Rapture CLI, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                      *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in complance    *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.cli

import rapture.core._

object Encoder {
  private val Translations = "<>#{}|\\^~[]`/?:@=&$!*:; ".to[Vector]

  def encode(s: String): String = s flatMap { c =>
    if(Translations contains c) {
      "%"+(c/16 + c/160*7 + 48).toChar+(c%16 + c%16/10*7 + 48).toChar
    } else c.toString
  }
}

object Compadd {

  def maxWidths(lists: Vector[Vector[String]]): Vector[Int] =
    lists.map(_.map(_.length)).reduce(_ zip _ map (Math.max _).tupled)

  def padRows(lists: Vector[Vector[String]]): Vector[String] = {
    val widths = maxWidths(lists)
    lists.map(_.zip(widths).map { case (s, w) => s.padTo(w, ' ') }.mkString("  "))
  }

  def apply(groupTitle: Option[String] = None, completions: Vector[String] = Vector(),
      columns: Boolean = true, descriptions: String => Vector[String] = _ => Vector(),
      colWidth: Int = 1000, hidden: Boolean): Vector[String] = {
    
    val display: Option[Vector[String]] = {
      val ds: Vector[Vector[String]] = completions.to[Vector] map descriptions
      if(ds.forall(_.isEmpty)) None
      else Some(padRows(completions zip ds map { case (c, d) => c +: d }))
    }
    
    display.toVector.flatten.map(s => "let "+Encoder.encode(s.take(colWidth - 1))) :+ Vector.strap(
      "compadd",
      groupTitle.to[Vector].flatMap(Seq("-J", _)),
      if(columns) Some("-l") else None,
      if(hidden) None else Some("-d matches"),
      if(hidden) Some("-n") else None,
      "--",
      completions.map(Encoder.encode)
    ).mkString(" ")
  }
}
