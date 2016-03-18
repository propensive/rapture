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

trait Deletable[Res] {
  def delete(res: Res): Unit
}

object Deletable {

  case class Summary(deleted: Int) {
    override def toString = s"$deleted file(s) deleted"
  }

  class Capability[Res](res: Res) {
    def delete()(implicit mode: Mode[`Deletable#delete`],
        deletable: Deletable[Res]): mode.Wrap[Summary, Exception] = mode wrap {
      deletable.delete(res)
      Summary(1)
    }
  }
}
