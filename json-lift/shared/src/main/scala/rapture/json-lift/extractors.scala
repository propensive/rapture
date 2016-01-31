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
package rapture.json.jsonBackends.lift

import rapture.json._
import rapture.data._

import net.liftweb.json._
import JsonAST._

private[lift] trait Extractors {
  implicit val liftJValueExtractor: JsonCastExtractor[JValue] =
    JsonCastExtractor(LiftAst, DataTypes.Any)
  
  implicit val liftJStringExtractor: JsonCastExtractor[JString] =
    JsonCastExtractor(LiftAst, DataTypes.String)
  
  implicit val liftJIntExtractor: JsonCastExtractor[JInt] =
    JsonCastExtractor(LiftAst, DataTypes.Number)
  
  implicit val liftJDoubleExtractor: JsonCastExtractor[JDouble] =
    JsonCastExtractor(LiftAst, DataTypes.Number)
  
  implicit val liftJArrayExtractor: JsonCastExtractor[JArray] =
    JsonCastExtractor(LiftAst, DataTypes.Array)
  
  implicit val liftJObjectExtractor: JsonCastExtractor[JObject] =
    JsonCastExtractor(LiftAst, DataTypes.Object)
}
