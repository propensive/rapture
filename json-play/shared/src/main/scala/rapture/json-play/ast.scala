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
package rapture.json.jsonBackends.play

import rapture.core._
import rapture.json._
import rapture.data.DataTypes

import scala.collection.mutable.{ListBuffer, HashMap}
import scala.collection.JavaConverters

import play.api.libs.json.{Json => PJson, _}

private[play] object PlayAst extends JsonBufferAst {

  override def toString = "<PlayAst>"

  override def dereferenceObject(obj: Any, element: String): Any = obj match {
    case obj@JsObject(_) => obj \ element match {
      case JsDefined(v) => v
      case _ => throw MissingValueException()
    }
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }
  
  override def getKeys(obj: Any): Iterator[String] =
    obj match {
      case obj: JsObject => obj.keys.iterator
      case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
    }
  
  override def dereferenceArray(array: Any, element: Int): Any =
    array match {
      case arr@JsArray(_) => arr(element) match {
        case JsDefined(v) => v
        case _ => throw MissingValueException()
      }
      case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
    }

  def getArray(array: Any): List[Any] = array match {
    case JsArray(arr) => arr.to[List]
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
  }

  def getBoolean(boolean: Any): Boolean = boolean match {
    case JsBoolean(v) => v
    case _ => throw TypeMismatchException(getType(boolean), DataTypes.Boolean)
  }
  
  def getBigDecimal(bigDecimal: Any): BigDecimal = bigDecimal match {
    case JsNumber(n) => n
    case _ => throw TypeMismatchException(getType(bigDecimal), DataTypes.Number)
  }
  
  def getDouble(double: Any): Double = double match {
    case JsNumber(n) => n.toDouble
    case _ => throw TypeMismatchException(getType(double), DataTypes.Number)
  }
  
  def getString(string: Any): String = string match {
    case JsString(s) => s
    case _ => throw TypeMismatchException(getType(string), DataTypes.String)
  }
  
  def getObject(obj: Any): Map[String, Any] = obj match {
    case JsObject(obj) => obj.toMap
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }
  
  def setObjectValue(obj: Any, name: String, value: Any): Any =
    (value, obj) match {
      case (value: JsValue, obj: JsValue) => PJson.toJson(obj.as[Map[String, JsValue]].updated(name, value))
    }
  
  def removeObjectValue(obj: Any, name: String): Any = obj match {
    case obj: JsObject => PJson.toJson(obj.as[Map[String, JsValue]] - name)
  }
  
  def addArrayValue(array: Any, value: Any): Any = array match {
    case v: JsValue => PJson.toJson(v.as[Array[JsValue]] :+ value.asInstanceOf[JsValue])
  }
  
  def setArrayValue(array: Any, index: Int, value: Any): Any = array match {
    case v: JsValue =>
      val array = v.as[Array[JsValue]]
      PJson.toJson(array.padTo(index, JsNull: JsValue).patch(index, Seq(value.asInstanceOf[JsValue]), 1))
  }
  
  def isArray(array: Any): Boolean = try {
    array match {
      case JsArray(_) => true
      case _ => false
    }
  } catch { case e: Exception => false }

  def isBoolean(boolean: Any): Boolean = try {
    boolean match {
      case JsBoolean(boolean) => true
      case _ => false
    }
  } catch { case e: Exception => false }

  def isNumber(num: Any): Boolean = try {
    num match {
      case JsNumber(_) => true
      case _ => false
    }
  } catch { case e: Exception => false }

  def isString(string: Any): Boolean = try {
    string match {
      case JsString(s) => true
      case _ => false
      //case JsDefined(string) => string.asOpt[String].isDefined
    }
  } catch { case e: Exception => false }

  def isObject(obj: Any): Boolean = try {
    obj match {
      case JsObject(obj) => true
      case _ => false
    }
  } catch { case e: Exception => false }
  
  def isNull(obj: Any): Boolean = obj match {
    case JsDefined(JsNull) => true
    case _ => false
  }
  
  val nullValue: Any = JsNull
  
  def fromArray(array: Seq[Any]): Any = PJson.toJson(array.map(_.asInstanceOf[JsValue]))
  def fromBoolean(boolean: Boolean): Any = PJson.toJson(boolean)
  def fromDouble(number: Double): Any = PJson.toJson(number)
  def fromBigDecimal(number: BigDecimal): Any = PJson.toJson(number)
  
  def fromObject(obj: Map[String, Any]): Any =
    PJson.toJson(obj.map { case (k, v: JsValue) => (k, v) })
  
  def fromString(string: String): Any = PJson.toJson(string)

}
