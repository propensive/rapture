/******************************************************************************************************************\
* Rapture JSON, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                     *
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
package rapture.json

import rapture.core._
import rapture.data._

object `package` {

  val patternMatching = rapture.data.patternMatching

  type Extractor[T, -D] = rapture.data.Extractor[T, D]
  type Serializer[T, -D] = rapture.data.Serializer[T, D]
  type DataGetException = rapture.data.DataGetException
  type TypeMismatchException = rapture.data.TypeMismatchException
  type MissingValueException = rapture.data.MissingValueException

  val TypeMismatchException = rapture.data.TypeMismatchException
  val MissingValueException = rapture.data.MissingValueException

  implicit def jsonStringContext(sc: StringContext)(implicit parser: Parser[String, JsonAst]) =
    new JsonStrings(sc)
  
  implicit def jsonBufferStringContext(sc: StringContext)
      (implicit parser: Parser[String, JsonBufferAst]) =
    new JsonBufferStrings(sc)
}
