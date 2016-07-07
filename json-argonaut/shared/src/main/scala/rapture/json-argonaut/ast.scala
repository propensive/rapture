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

package rapture.json.jsonBackends.argonaut

import rapture.core._
import rapture.data.DataTypes
import rapture.json._

import argonaut.{Json => AJson, _}
import Argonaut._

private[argonaut] object ArgonautAst extends JsonBufferAst {

  override def toString = "<ArgonautAst>"

  override def dereferenceObject(obj: Any, element: String): Any =
    obj match {
      case j: AJson if j.isObject => j.field(element).getOrElse(throw MissingValueException())
      case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
    }

  override def getKeys(obj: Any): Iterator[String] =
    obj match {
      case j: AJson if j.isObject => j.objectFields.get.iterator
      case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
    }

  override def dereferenceArray(array: Any, element: Int): Any =
    array match {
      case j: AJson if j.isArray => j.array.get(element)
      case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
    }

  def getArray(array: Any): List[Any] = array match {
    case j: AJson if j.isArray => j.array.get
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
  }

  def getBoolean(boolean: Any): Boolean = boolean match {
    case j: AJson if j.isBool => j.bool.get
    case _ => throw TypeMismatchException(getType(boolean), DataTypes.Boolean)
  }

  def getDouble(double: Any): Double = double match {
    case j: AJson if j.isNumber => j.number.get.toDouble.get
    case _ => throw TypeMismatchException(getType(double), DataTypes.Number)
  }

  def getBigDecimal(bigDecimal: Any): BigDecimal = bigDecimal match {
    case j: AJson if j.isNumber => BigDecimal(j.number.get.toBigInt.get)
    case _ => throw TypeMismatchException(getType(bigDecimal), DataTypes.Number)
  }

  def getString(string: Any): String = string match {
    case j: AJson if j.isString => j.string.get
    case _ => throw TypeMismatchException(getType(string), DataTypes.String)
  }

  def getObject(obj: Any): Map[String, Any] = obj match {
    case j: AJson if j.isObject => j.obj.get.toMap.map { case (k, v) => k.toString -> v }
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }

  def setObjectValue(obj: Any, name: String, value: Any): Any = {
    val contents = (name, value) :: obj.asInstanceOf[AJson].obj.get.toList.collect {
      case (k, v) if k.toString != name => k.toString -> v
    }
    fromObject(contents.toMap)
  }

  def removeObjectValue(obj: Any, name: String): Any = {
    val contents = obj.asInstanceOf[AJson].obj.get.toList.collect {
      case (k, v) if k.toString != name => k.toString -> v
    }
    fromObject(contents.toMap)
  }

  def addArrayValue(array: Any, value: Any): Any =
    fromArray(array.asInstanceOf[AJson].array.get :+ value)

  def setArrayValue(array: Any, index: Int, value: Any): Any =
    fromArray(array.asInstanceOf[AJson].array.get.padTo(index, nullValue).patch(index, Seq(value), 1))

  def isArray(array: Any): Boolean = array match {
    case j: AJson if j.isArray => true
    case _ => false
  }

  def isBoolean(boolean: Any): Boolean = boolean match {
    case j: AJson if j.isBool => true
    case _ => false
  }

  def isNumber(num: Any): Boolean = num match {
    case j: AJson if j.isNumber => true
    case _ => false
  }

  def isString(string: Any): Boolean = string match {
    case j: AJson if j.isString => true
    case _ => false
  }

  def isObject(obj: Any): Boolean = obj match {
    case j: AJson if j.isObject => true
    case _ => false
  }

  def isNull(obj: Any): Boolean = obj match {
    case j: AJson if j.isNull => true
    case _ => false
  }

  def fromArray(array: Seq[Any]): Any = jArray(array.to[List] map { case v: AJson => v })
  def fromBoolean(boolean: Boolean): Any = jBool(boolean)
  def fromDouble(number: Double): Any = jNumber(number).get
  def fromBigDecimal(number: BigDecimal): Any = jNumber(number.toDouble).get

  def fromObject(obj: Map[String, Any]): Any =
    AJson(obj.mapValues { case v: AJson => v }.to[List]: _*)

  def fromString(string: String): Any = jString(string)

  // FIXME: Is there a better way of getting a JNull?
  lazy val nullValue: Any = argonaut.Parse.parseOption("null").get

}
