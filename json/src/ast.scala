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

import scala.util.Try
import scala.collection.JavaConverters

/** Represents a JSON ast implementation which is used throughout this library */
trait JsonAst extends DataAst {

  def isScalar(any: Any) = isBoolean(any) || isNumber(any) || isString(any)

  /** Extracts a `Boolean` from the parsed JSON. */
  def getBoolean(boolean: Any): Boolean

  def fromBoolean(boolean: Boolean): Any

  /** Extracts a `String` from the parsed JSON. */
  def getString(string: Any): String

  def fromString(string: String): Any

  /** Extracts a `Double` from the parsed JSON. */
  def getDouble(number: Any): Double

  def fromDouble(number: Double): Any

  /** Extracts a `BigDecimal` from the parsed JSON. */
  def getBigDecimal(number: Any): BigDecimal
  
  def fromBigDecimal(number: BigDecimal): Any

  /** Tests if the element represents a `Boolean` */
  def isBoolean(any: Any): Boolean
  
  /** Tests if the element represents a `String` */
  def isString(any: Any): Boolean
  
  /** Tests if the element represents a number */
  def isNumber(any: Any): Boolean
  
  /** Tests if the element represents a `null` */
  def isNull(any: Any): Boolean

  /** The value used to represent a `null` */
  def nullValue: Any

  /** Returns the DataType instance for the particular type. */
  def getType(any: Any): DataTypes.DataType =
    if(isBoolean(any)) DataTypes.Boolean
    else if(isString(any)) DataTypes.String
    else if(isNumber(any)) DataTypes.Number
    else if(isObject(any)) DataTypes.Object
    else if(isArray(any)) DataTypes.Array
    else if(isNull(any)) DataTypes.Null
    else throw MissingValueException()

  protected def typeTest(pf: PartialFunction[Any, Unit])(v: Any) = pf.isDefinedAt(v)
}

trait JsonBufferAst extends JsonAst with MutableDataAst

