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

package rapture.io

import rapture.uri._

trait HasResourceName[Res] {
  def resourceName(res: Res): String
  def resourceExtension(res: Res): Option[String] = {
    val parts = resourceName(res).split("\\.")
    if(parts.length > 1) Some(parts.last) else None
  }
}

object HasResourceName {
  
  implicit val classpathHasResourceName: HasResourceName[ClasspathUrl] = new HasResourceName[ClasspathUrl] {
    def resourceName(classpathUrl: ClasspathUrl): String = classpathUrl.elements.last
  }

  class Capability[Res](res: Res) {
    def resourceName(implicit hasResourceName: HasResourceName[Res]): String =
      hasResourceName.resourceName(res)
    
    def resourceExtension(implicit hasResourceName: HasResourceName[Res]): Option[String] =
      hasResourceName.resourceExtension(res)
  }
}
