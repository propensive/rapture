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

package rapture.json.jsonBackends.jackson
import rapture.core._
import rapture.json._
import rapture.data.DataTypes
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.node.NullNode

import scala.collection.JavaConversions._

/** A type class for Jackson parsing */
private[jackson] object JacksonAst extends JsonAst {

  override def toString = "<JacksonAst>"

  private val mapper = new ObjectMapper()
    .enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS)
    .enable(DeserializationFeature.USE_BIG_INTEGER_FOR_INTS)

  def getArray(array: Any): List[Any] = array match {
    case list: JsonNode if list.isArray => list.elements.to[List]
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
  }

  def getBoolean(boolean: Any): Boolean = boolean match {
    case boolean: JsonNode if boolean.isBoolean => boolean.asBoolean
    case _ => throw TypeMismatchException(getType(boolean), DataTypes.Boolean)
  }
  
  def getDouble(number: Any): Double = number match {
    case number: JsonNode if number.isBigDecimal => number.decimalValue.doubleValue
    case number: JsonNode if number.isBigInteger => number.bigIntegerValue.doubleValue
    case number: JsonNode if number.isNumber => number.asDouble
    case number: Double => number
    case _ => throw TypeMismatchException(getType(number), DataTypes.Number)
  }
  
  def getBigDecimal(number: Any): BigDecimal = number match {
    case number: JsonNode if number.isBigDecimal => BigDecimal(number.decimalValue)
    case number: JsonNode if number.isBigInteger => BigDecimal(number.bigIntegerValue)
    case number: JsonNode if number.isNumber => number.asDouble
    case number: Double => BigDecimal(number)
    case _ => throw TypeMismatchException(getType(number), DataTypes.Number)
  }
  
  def getString(string: Any): String = string match {
    case string: JsonNode if string.isTextual => string.asText
    case string: String => string
    case _ => throw TypeMismatchException(getType(string), DataTypes.String)
  }
  
  def getObject(obj: Any): Map[String, Any] = obj match {
    case obj: JsonNode if obj.isObject =>
      (obj.fieldNames map { case k => k -> Option(obj.get(k)).get }).toMap
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }
  
  override def dereferenceObject(obj: Any, element: String): Any = obj match {
    case obj: JsonNode if obj.isObject => Option(obj.get(element)).get
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }

  override def getKeys(obj: Any): Iterator[String] = obj match {
    case obj: JsonNode if obj.isObject => obj.fieldNames.to[Iterator]
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }

  override def dereferenceArray(array: Any, element: Int): Any = array match {
    case array: JsonNode if array.isArray => array.get(element)
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
  }

  def setObjectValue(obj: JsonNode, name: String, value: JsonNode): Unit = obj match {
    case obj: node.ObjectNode => obj.set(name, value)
  }
  
  def removeObjectValue(obj: JsonNode, name: String): Unit = obj match {
    case obj: node.ObjectNode => obj.remove(name)
  }
  
  def addArrayValue(array: JsonNode, value: JsonNode): Unit = array match {
    case array: node.ArrayNode => array.add(value)
  }
  
  def setArrayValue(array: JsonNode, index: Int, value: JsonNode): Unit = ???

  def nullValue = NullNode.instance

  def fromArray(array: Seq[Any]): Any = {
    val newArray = mapper.createArrayNode
    for(v <- array) v match {
      case v: Boolean => newArray.add(v)
      case v: String => newArray.add(v)
      case v: Double => newArray.add(v)
      case v: JsonNode => newArray.add(v)
    }
    newArray
  }

  def fromBoolean(boolean: Boolean): Any = boolean
  def fromDouble(number: Double): Any = number
  def fromBigDecimal(number: BigDecimal): Any = number.toDouble
  
  def fromObject(obj: Map[String,Any]): Any = {
    val newObject = mapper.createObjectNode
    for((k, v) <- obj) v match {
      case v: Boolean => newObject.put(k, v)
      case v: String => newObject.put(k, v)
      case v: Double => newObject.put(k, v)
      case v: JsonNode => newObject.set(k, v)
      case null => newObject.putNull(k)
    }
    newObject
  }
  def fromString(string: String): Any = string


  def isBoolean(any: Any): Boolean = any match {
    case x: JsonNode if x.isBoolean => true
    case _ => false
  }
  
  def isString(any: Any): Boolean = any match {
    case x: JsonNode if x.isTextual => true
    case x: String => true
    case _ => false
  }

  def isNumber(any: Any): Boolean = any match {
    case x: JsonNode if x.isNumber => true
    case x: Double => true
    case _ => false
  }
  
  def isObject(any: Any): Boolean = any match {
    case x: JsonNode if x.isObject => true
    case _ => false
  }
  
  def isArray(any: Any): Boolean = any match {
    case x: JsonNode if x.isArray => true
    case _ => false
  }
  
  def isNull(any: Any): Boolean = any match {
    case x: JsonNode if x.isNull => true
    case _ => false
  }
}
