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

package rapture.uri

object UriCapable {
  class Capability[Res: UriCapable](res: Res) {
    def uri: Uri = implicitly[UriCapable[Res]].uri(res)
  }
}

case class Uri(scheme: String, schemeSpecificPart: String) {
  override def toString: String = s"$scheme:$schemeSpecificPart"
}

trait UriCapable[-Res] {
  def uri(res: Res): Uri
}

object Linkable {
  class Capability[Res: Linkable](res: Res) {
    def link: PathLink = implicitly[Linkable[Res]].link(res)
  }

  implicit def linkableUri[Res: UriCapable]: Linkable[Res] = new Linkable[Res] {
    def link(res: Res): PathLink = PathLink(implicitly[UriCapable[Res]].uri(res).toString)
  }
}

object PathLink {
  implicit def linkLinkable: Linkable[PathLink] = new Linkable[PathLink] {
    def link(lnk: PathLink): PathLink = lnk
  }
}

case class PathLink(link: String) {
  override def toString: String = link
}

trait Linkable[-Res] {
  def link(res: Res): PathLink
}
