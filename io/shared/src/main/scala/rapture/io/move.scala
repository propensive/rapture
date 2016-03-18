/******************************************************************************************************************\
* Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.                                          *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance   *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.io
import rapture.core._

trait Movable[FromType, ToType] {
  def move(from: FromType, to: ToType): Movable.Summary
}

object Movable {
  case class Summary(streamed: Option[Long]) {
    override def toString = streamed match {
      case None => "moved file"
      case Some(b) => s"streamed, deleted $b bytes"
    }
  }

  class Capability[FromType](from: FromType) {
    def moveTo[ToType](to: ToType)(implicit mode: Mode[`Movable#moveTo`],
        movable: Movable[FromType, ToType]): mode.Wrap[Summary, Exception] =
      mode.wrap(?[Movable[FromType, ToType]].move(from, to))
  }
}
