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
package rapture.json.jsonBackends.jawn

import rapture.data._
import rapture.json._

import _root_.jawn.ast._

private[jawn] trait Extractors {
  implicit val jawnJObjectExtractor: JsonCastExtractor[JObject] =
    JsonCastExtractor(JawnAst, DataTypes.Object)
  
  implicit val jawnJArrayExtractor: JsonCastExtractor[JArray] =
    JsonCastExtractor(JawnAst, DataTypes.Array)
  
  implicit val jawnDeferNumExtractor: JsonCastExtractor[DeferNum] =
    JsonCastExtractor(JawnAst, DataTypes.Number)
  
  implicit val jawnDoubleNumExtractor: JsonCastExtractor[DoubleNum] =
    JsonCastExtractor(JawnAst, DataTypes.Number)
  
  implicit val jawnLongNumExtractor: JsonCastExtractor[LongNum] =
    JsonCastExtractor(JawnAst, DataTypes.Number)
  
  implicit val jawnJNumExtractor: JsonCastExtractor[JNum] =
    JsonCastExtractor(JawnAst, DataTypes.Number)
  
  implicit val jawnStringExtractor: JsonCastExtractor[JString] =
    JsonCastExtractor(JawnAst, DataTypes.String)
  
  implicit val jawnAtomExtractor: JsonCastExtractor[JAtom] =
    JsonCastExtractor(JawnAst, DataTypes.Scalar)
  
  implicit val jawnValueExtractor: JsonCastExtractor[JValue] =
    JsonCastExtractor(JawnAst, DataTypes.Any)
}
