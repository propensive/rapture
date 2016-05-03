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

package rapture.json.jsonBackends.jawn

import rapture.core._
import rapture.json._
import rapture.data.DataTypes
import rapture.data.TypeMismatchException
import rapture.data.MissingValueException

import jawn.ast._

private[jawn] object JawnAst extends JsonBufferAst {

  override def dereferenceObject(obj: Any, element: String): Any =
    obj match {
      case JObject(obj) =>
        try obj(element) catch {
          case e: Exception => throw MissingValueException()
        }
      case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
    }

  override def getKeys(obj: Any): Iterator[String] =
    obj match {
      case JObject(obj) => obj.keys.iterator
      case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
    }

  override def dereferenceArray(array: Any, element: Int): Any =
    array match {
      case JArray(arr) => arr(element)
      case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
    }

  def getArray(array: Any): List[Any] = array match {
    case JArray(xs) => xs.toList
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
  }

  def getBoolean(boolean: Any): Boolean = boolean match {
    case boolean: Boolean => boolean
    case JTrue => true
    case JFalse => false
    case _ => throw TypeMismatchException(getType(boolean), DataTypes.Boolean)
  }

  def getBigDecimal(bigDecimal: Any): BigDecimal = bigDecimal match {
    case DoubleNum(d) => BigDecimal(d)
    case DeferLong(v) => BigDecimal(v)
    case DeferNum(v) => BigDecimal(v)
    case _ =>
      throw TypeMismatchException(getType(bigDecimal), DataTypes.Number)
  }

  def getDouble(double: Any): Double = double match {
    case DoubleNum(d) => d
    case DeferLong(v) => v.toDouble
    case DeferNum(v) => java.lang.Double.valueOf(v)
    case _ => throw TypeMismatchException(getType(double), DataTypes.Number)
  }

  def getString(string: Any): String = string match {
    case JString(s) => s
    case _ => throw TypeMismatchException(getType(string), DataTypes.String)
  }

  def getObject(obj: Any): Map[String, Any] = obj match {
    case JObject(o) => o.toMap
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }

  def setObjectValue(obj: Any, name: String, value: Any): Any = {
    obj.asInstanceOf[JObject].vs(name) = value.asInstanceOf[JValue]
    obj
  }

  def removeObjectValue(obj: Any, name: String): Any = {
    obj.asInstanceOf[JObject].vs -= name
    obj
  }

  def addArrayValue(array: Any, value: Any): Any =
    JArray(array.asInstanceOf[JArray].vs :+ value.asInstanceOf[JValue])

  def setArrayValue(array: Any, index: Int, value: Any): Any =
    JArray(
        array
          .asInstanceOf[JArray]
          .vs
          .padTo(index, null)
          .patch(index, Seq(value.asInstanceOf[JValue]), 1))

  def isArray(array: Any): Boolean = array match {
    case JArray(xs) => true
    case _ => false
  }

  def isBoolean(boolean: Any): Boolean = boolean match {
    case JTrue | JFalse => true
    case _ => false
  }

  def isNumber(num: Any): Boolean = num match {
    case DoubleNum(_) | DeferLong(_) | DeferNum(_) => true
    case _ => false
  }

  def isString(string: Any): Boolean = string match {
    case JString(_) => true
    case _ => false
  }

  def isObject(obj: Any): Boolean = obj match {
    case JObject(_) => true
    case _ => false
  }

  def isNull(obj: Any): Boolean = obj match {
    case JNull => true
    case _ => false
  }

  def fromArray(array: Seq[Any]): Any =
    JArray(array.to[Array] map { case v: JValue => v })
  def fromBoolean(boolean: Boolean): Any = if (boolean) JTrue else JFalse
  def fromDouble(number: Double): Any = DoubleNum(number)
  def fromBigDecimal(number: BigDecimal): Any = DeferNum(number.toString)

  def fromObject(obj: Map[String, Any]): Any =
    JObject(
        collection.mutable.Map(
            obj.mapValues { case v: JValue => v }.to[Seq]: _*))

  def fromString(string: String): Any = JString(string)

  val nullValue = JNull

  override def toString = "<Jawn>"
}
