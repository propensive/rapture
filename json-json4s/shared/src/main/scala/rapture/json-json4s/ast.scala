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

import rapture.data.DataTypes
import rapture.json._

import org.json4s._

private[json4s] object Json4sAst extends JsonBufferAst {

  import JsonAST._

  override def toString = "<Json4sAst>"

  def getArray(array: Any): List[Any] = array match {
    case JArray(xs)=> xs.toList
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
  }

  def fromArray(array: Seq[Any]): Any = JArray(array.to[List] map { case v: JValue => v })
  
  def getBoolean(boolean: Any): Boolean = boolean match {
    case boolean: Boolean => boolean
    case JBool(v) => v
    case _ => throw TypeMismatchException(getType(boolean), DataTypes.Boolean)
  }
  
  def fromBoolean(boolean: Boolean): Any = JBool(boolean)
  
  def getDouble(double: Any): Double = double match {
    case JDouble(d) => d
    case JInt(v) => v.toDouble
    case JDecimal(v) => v.toDouble
    case _ => throw TypeMismatchException(getType(double), DataTypes.Number)
  }
  
  def getBigDecimal(bigDecimal: Any): BigDecimal = bigDecimal match {
    case JDecimal(v) => v
    case JDouble(d) => BigDecimal(d)
    case JInt(v) => BigDecimal(v)
    case _ => throw TypeMismatchException(getType(bigDecimal), DataTypes.Number)
  }
  
  def fromDouble(number: Double): Any = JDouble(number)

  def fromBigDecimal(number: BigDecimal): Any = JDecimal(number)
  
  def getString(string: Any): String = string match {
    case JString(s) => s
    case _ => throw TypeMismatchException(getType(string), DataTypes.String)
  }
  
  def fromString(string: String): Any = JString(string)

  def getObject(obj: Any): Map[String, Any] = obj match {
    case JObject(o) => o.toMap
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }
  
  def fromObject(obj: Map[String,Any]): Any =
    JObject(obj.mapValues{ case v: JValue => v }.to[List])
  
  def setObjectValue(obj: Any, name: String, value: Any): Any = {
    val contents = (name, value) :: obj.asInstanceOf[JObject].obj.filter(_._1 != name)
    JObject(contents map { case (k: String, v: JValue) => k -> v })
  }
  
  def removeObjectValue(obj: Any, name: String): Any =
    JObject(obj.asInstanceOf[JObject].obj.filter(_._1 == name))
  
  def addArrayValue(array: Any, value: Any): Any =
    JArray(array.asInstanceOf[JArray].arr :+ value.asInstanceOf[JValue])
  
  def setArrayValue(array: Any, index: Int, value: Any): Any = array match {
    case array: JArray =>
      JArray(array.arr.padTo(index, JNull).patch(index, Seq(value.asInstanceOf[JValue]), 1))
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
  }
  
  def isBoolean(boolean: Any): Boolean = boolean match {
    case JBool(_) => true
    case _ => false
  }
  
  def isString(string: Any): Boolean = string match {
    case JString(_) => true
    case _ => false
  }
  
  def isNumber(num: Any): Boolean = num match {
    case JDecimal(_) | JInt(_) | JDouble(_) => true
    case _ => false
  }
  
  def isObject(obj: Any): Boolean = obj match {
    case JObject(_) => true
    case _ => false
  }
  
  def isArray(array: Any): Boolean = array match {
    case JArray(xs)=> true
    case _ => false
  }

  def isNull(obj: Any): Boolean = obj match {
    case JNull => true
    case _ => false
  }
 
  def nullValue: Any = JNull

  override def dereferenceObject(obj: Any, element: String): Any =
    obj match {
      case JObject(obj) => obj.find(_._1 == element).get._2
      case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
    }
  
  override def getKeys(obj: Any): Iterator[String] =
    obj match {
      case JObject(obj) => obj.map(_._1).iterator
      case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
    }
  
  override def dereferenceArray(array: Any, element: Int): Any =
    array match {
      case JArray(arr) => arr(element)
      case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
    }
}
