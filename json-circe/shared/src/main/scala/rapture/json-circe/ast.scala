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

package rapture.json.jsonBackends.circe

import rapture.core._
import rapture.data.DataTypes
import rapture.json._

import io.circe.{Json => CirceJson}

private[circe] object CirceAst extends JsonBufferAst {

  override def toString = "<Circe>"

  override def dereferenceObject(obj: Any, element: String): Any = {
    def fail(): Nothing = throw TypeMismatchException(getType(obj), DataTypes.Object)
    obj match {
      case j: CirceJson => j.asObject.getOrElse(fail())(element).get
      case _ => fail()
    }
  }

  override def getKeys(obj: Any): Iterator[String] = {
    def fail(): Nothing = throw TypeMismatchException(getType(obj), DataTypes.Object)
    obj match {
      case j: CirceJson => j.asObject.getOrElse(fail()).fields.to[Iterator]
      case _ => fail()
    }
  }

  override def dereferenceArray(j: Any, elem: Int): Any = j match {
    case j: CirceJson if j.isArray => j.asArray.get(elem)
    case _ => throw TypeMismatchException(getType(j), DataTypes.Array)
  }

  def getArray(j: Any): List[Any] = j match {
    case j: CirceJson if j.isArray => j.asArray.get.to[List]
    case _ => throw TypeMismatchException(getType(j), DataTypes.Array)
  }

  def getBoolean(j: Any): Boolean = j match {
    case j: CirceJson if j.isBoolean => j.asBoolean.get
    case _ => throw TypeMismatchException(getType(j), DataTypes.Boolean)
  }

  def getDouble(double: Any): Double = double match {
    case j: CirceJson if j.isNumber => j.asNumber.get.toDouble
    case _ => throw TypeMismatchException(getType(double), DataTypes.Number)
  }

  def getBigDecimal(bigDecimal: Any): BigDecimal = bigDecimal match {
    case j: CirceJson if j.isNumber =>
      j.asNumber.get.toBigDecimal.getOrElse(throw TypeMismatchException(getType(bigDecimal), DataTypes.Number))
    case _ =>
      throw TypeMismatchException(getType(bigDecimal), DataTypes.Number)
  }

  def getString(string: Any): String = string match {
    case j: CirceJson if j.isString => j.asString.get
    case _ => throw TypeMismatchException(getType(string), DataTypes.String)
  }

  def getObject(obj: Any): Map[String, Any] = obj match {
    case j: CirceJson if j.isObject => j.asObject.get.toMap.map { case (k, v) => k.toString -> v }
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }

  def setObjectValue(obj: Any, name: String, value: Any): Any = {
    val contents = (name, value) :: obj.asInstanceOf[CirceJson].asObject.get.toList.collect {
      case (k, v) if k.toString != name => k.toString -> v
    }
    fromObject(contents.toMap)
  }

  def removeObjectValue(obj: Any, name: String): Any = {
    val contents = obj.asInstanceOf[CirceJson].asObject.get.toList.collect {
      case (k, v) if k.toString != name => k.toString -> v
    }
    fromObject(contents.toMap)
  }

  def addArrayValue(array: Any, value: Any): Any =
    fromArray(array.asInstanceOf[CirceJson].asArray.get :+ value)

  def setArrayValue(array: Any, index: Int, value: Any): Any =
    fromArray(array.asInstanceOf[CirceJson].asArray.get.padTo(index, nullValue).patch(index, Seq(value), 1))

  def isArray(array: Any): Boolean = array match {
    case j: CirceJson if j.isArray => true
    case _ => false
  }

  def isBoolean(boolean: Any): Boolean = boolean match {
    case j: CirceJson if j.isBoolean => true
    case _ => false
  }

  def isNumber(num: Any): Boolean = num match {
    case j: CirceJson if j.isNumber => true
    case _ => false
  }

  def isString(string: Any): Boolean = string match {
    case j: CirceJson if j.isString => true
    case _ => false
  }

  def isObject(obj: Any): Boolean = obj match {
    case j: CirceJson if j.isObject => true
    case _ => false
  }

  def isNull(obj: Any): Boolean = obj match {
    case j: CirceJson if j.isNull => true
    case _ => false
  }

  def fromArray(array: Seq[Any]): Any = CirceJson.arr(array.to[List].map { case v: CirceJson => v }: _*)
  def fromBoolean(boolean: Boolean): Any = CirceJson.fromBoolean(boolean)
  def fromDouble(number: Double): Any = CirceJson.fromDouble(number).get
  def fromBigDecimal(number: BigDecimal): Any = CirceJson.fromBigDecimal(number)

  def fromObject(obj: Map[String, Any]): Any =
    CirceJson.obj(obj.mapValues { case v: CirceJson => v }.to[List]: _*)

  def fromString(string: String): Any = CirceJson.fromString(string)

  // FIXME: Is there a better way of getting a JNull?
  lazy val nullValue: Any = CirceJson.Null

}
