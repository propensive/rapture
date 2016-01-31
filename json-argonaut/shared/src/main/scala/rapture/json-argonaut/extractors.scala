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
package rapture.json.jsonBackends.argonaut

import rapture.json._
import rapture.data._

import argonaut.{Json => AJson, _}

private[argonaut] trait Extractors {
  implicit val argonautJObjectExtractor: JsonCastExtractor[JsonObject] =
    JsonCastExtractor(ArgonautAst, DataTypes.Object)
  
  implicit val argonautJValueExtractor: JsonCastExtractor[AJson] =
    JsonCastExtractor(ArgonautAst, DataTypes.Object)
}
