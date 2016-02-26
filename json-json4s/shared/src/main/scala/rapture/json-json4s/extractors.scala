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
package rapture.json.jsonBackends.json4s

import rapture.json._
import rapture.data._

import org.json4s._

private[json4s] trait Extractors {
  implicit val json4sJValueExtractor: JsonCastExtractor[JValue] =
    JsonCastExtractor(Json4sAst, DataTypes.Any)
  
  implicit val json4sJDecimalExtractor: JsonCastExtractor[JDecimal] =
    JsonCastExtractor(Json4sAst, DataTypes.Number)
  
  implicit val json4sJDoubleExtractor: JsonCastExtractor[JDouble] =
    JsonCastExtractor(Json4sAst, DataTypes.Number)
  
  implicit val json4sJStringExtractor: JsonCastExtractor[JString] =
    JsonCastExtractor(Json4sAst, DataTypes.String)
  
  implicit val json4sJIntExtractor: JsonCastExtractor[JInt] =
    JsonCastExtractor(Json4sAst, DataTypes.Number)
  
  implicit val json4sJArrayExtractor: JsonCastExtractor[JArray] =
    JsonCastExtractor(Json4sAst, DataTypes.Array)
  
  implicit val json4sJObjectExtractor: JsonCastExtractor[JObject] =
    JsonCastExtractor(Json4sAst, DataTypes.Object)
}
