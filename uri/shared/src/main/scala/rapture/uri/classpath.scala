/******************************************************************************************************************\
* Rapture URI, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                      *
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

package rapture.uri

import rapture.core._

object ClasspathUrl {
  implicit def cpSlashString: Dereferenceable[ClasspathUrl, String, ClasspathUrl] =
    new Dereferenceable[ClasspathUrl, String, ClasspathUrl] {
      def dereference(p1: ClasspathUrl, p2: String) = ClasspathUrl(p1.elements :+ p2)
    }
  
  implicit def cpSlashRelativePath[RP <: RelativePath]: Dereferenceable[ClasspathUrl, RP, ClasspathUrl] =
    new Dereferenceable[ClasspathUrl, RP, ClasspathUrl] {
      def dereference(p1: ClasspathUrl, p2: RP) = ClasspathUrl(p1.elements.dropRight(p2.ascent) ++ p2.elements)
    }

  implicit def uriCapable: UriCapable[ClasspathUrl] = new UriCapable[ClasspathUrl] {
    def uri(cp: ClasspathUrl) = Uri("classpath", cp.elements.mkString("/"))
  }
}

case class ClasspathUrl(elements: Vector[String]) {
  override def toString: String = s"classpath:${elements.mkString("/")}"
}
