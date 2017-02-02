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

package rapture.json.jsonBackends.spray

import rapture.core._
import rapture.json._

import spray.json._

import DefaultJsonProtocol._

private[spray] object SprayAst extends JsonBufferAst {

  override def toString = "<SprayAst>"

  override def dereferenceObject(obj: Any, element: String): Any =
    obj match {
      case obj: JsObject =>
        try obj.getFields(element).head
        catch {
          case e: IndexOutOfBoundsException => throw MissingValueException()
        }
      case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
    }

  override def getKeys(obj: Any): Iterator[String] =
    obj match {
      case obj: JsObject => obj.fields.keysIterator
      case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
    }

  override def dereferenceArray(array: Any, element: Int): Any =
    array match {
      case v: JsValue =>
        val arr = v.convertTo[Array[JsValue]]
        arr(element)
      case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
    }

  def getArray(array: Any): List[Any] = array match {
    case v: JsValue =>
      try v.convertTo[List[JsValue]]
      catch {
        case e: Exception => throw TypeMismatchException(getType(array), DataTypes.Array)
      }
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
  }

  def getBoolean(boolean: Any): Boolean = boolean match {
    case v: JsValue =>
      try v.convertTo[Boolean]
      catch {
        case e: Exception => throw TypeMismatchException(getType(boolean), DataTypes.Boolean)
      }
    case _ => throw TypeMismatchException(getType(boolean), DataTypes.Boolean)
  }

  def getBigDecimal(bigDecimal: Any): BigDecimal = bigDecimal match {
    case v: JsValue =>
      try v.convertTo[BigDecimal]
      catch {
        case e: Exception => throw TypeMismatchException(getType(bigDecimal), DataTypes.Number)
      }
    case _ => throw TypeMismatchException(getType(bigDecimal), DataTypes.Number)
  }

  def getDouble(double: Any): Double = double match {
    case v: JsValue =>
      try v.convertTo[Double]
      catch {
        case e: Exception => throw TypeMismatchException(getType(double), DataTypes.Number)
      }
    case _ => throw TypeMismatchException(getType(double), DataTypes.Number)
  }

  def getString(string: Any): String = string match {
    case v: JsValue =>
      try v.convertTo[String]
      catch {
        case e: Exception => throw TypeMismatchException(getType(string), DataTypes.String)
      }
    case _ => throw TypeMismatchException(getType(string), DataTypes.String)
  }

  def getObject(obj: Any): Map[String, Any] = obj match {
    case v: JsObject => v.fields
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }

  def setObjectValue(obj: Any, name: String, value: Any): Any =
    (value, obj) match {
      case (value: JsValue, obj: JsValue) => obj.asJsObject.fields.updated(name, value).toJson
      case _ => ???
    }

  def removeObjectValue(obj: Any, name: String): Any = obj match {
    case obj: JsObject => (obj.fields - name).toJson
  }

  def addArrayValue(array: Any, value: Any): Any = array match {
    case v: JsValue => (v.convertTo[Array[JsValue]] :+ value.asInstanceOf[JsValue]).toJson
  }

  def setArrayValue(array: Any, index: Int, value: Any): Any = array match {
    case v: JsValue =>
      val array = v.convertTo[Array[JsValue]]
      array.padTo(index, JsNull: JsValue).patch(index, Seq(value.asInstanceOf[JsValue]), 1).toJson
  }

  def isArray(array: Any): Boolean =
    try {
      array match {
        case array: JsValue =>
          array.convertTo[Array[JsValue]]
          true
        case _ => false
      }
    } catch {
      case e: DeserializationException => false
    }

  def isBoolean(boolean: Any): Boolean =
    try {
      boolean match {
        case boolean: JsValue =>
          boolean.convertTo[Boolean]
          true
        case _ => false
      }
    } catch {
      case e: ClassCastException => false
      case e: DeserializationException => false
    }

  def isNumber(num: Any): Boolean =
    try {
      num match {
        case JsNull => false
        case num: JsValue =>
          num.convertTo[Double]
          true
        case _ => false
      }
    } catch {
      case e: ClassCastException => false
      case e: DeserializationException => false
    }

  def isString(string: Any): Boolean =
    try {
      string match {
        case string: JsValue =>
          string.convertTo[String]
          true
        case _ => false
      }
    } catch {
      case e: ClassCastException => false
      case e: DeserializationException => false
    }

  def isObject(obj: Any): Boolean = obj match {
    case obj: JsObject => true
    case _ => false
  }

  def isNull(obj: Any): Boolean = obj match {
    case JsNull => true
    case _ => false
  }

  def nullValue: Any = JsNull

  def fromArray(array: Seq[Any]): Any = array.map(_.asInstanceOf[JsValue]).toJson
  def fromBoolean(boolean: Boolean): Any = boolean.toJson
  def fromDouble(number: Double): Any = number.toJson
  def fromBigDecimal(number: BigDecimal): Any = number.toJson

  def fromObject(obj: Map[String, Any]): Any =
    obj.map {
      case (k, v: JsValue) => (k, v)
      case _ => ???
    }.toJson

  def fromString(string: String): Any = string.toJson

}
