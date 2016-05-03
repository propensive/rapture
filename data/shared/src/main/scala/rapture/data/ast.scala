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

package rapture.data

import rapture.core._

object DataTypes {
  sealed class DataType(val name: String)
  case object Number extends DataType("number")
  case object String extends DataType("string")
  case object Null extends DataType("null")
  case object Boolean extends DataType("boolean")
  case object Array extends DataType("array")
  case object Object extends DataType("object")
  case object Scalar extends DataType("scalar")
  case object Container extends DataType("container")
  case object Any extends DataType("any")
}

@implicitNotFound(
    msg = "Cannot find ${Ast} parser for values of type ${Source}")
trait Parser[-Source, +Ast <: DataAst] {
  val ast: Ast
  def parse(s: Source): Option[Any]
}

trait DataAst {

  /** Dereferences the named element within the JSON object. */
  def dereferenceObject(obj: Any, element: String): Any =
    getObject(obj)(element)

  /** Returns at `Iterator[String]` over the names of the elements in the JSON object. */
  def getKeys(obj: Any): Iterator[String] =
    getObject(obj).keys.iterator

  /** Gets the indexed element from the parsed JSON array. */
  def dereferenceArray(array: Any, element: Int): Any =
    getArray(array)(element)

  /** Tests if the element represents an `Object` */
  def isObject(any: Any): Boolean

  /** Tests if the element represents an `Array` */
  def isArray(any: Any): Boolean

  def isNull(any: Any): Boolean

  /** Extracts a JSON object as a `Map[String, Any]` from the parsed JSON. */
  def getObject(obj: Any): Map[String, Any]

  def getChildren(obj: Any): Seq[Any] = {
    val m = getObject(obj)
    getKeys(obj).to[List] map m
  }

  def fromObject(obj: Map[String, Any]): Any

  /** Extracts a JSON array as a `Seq[Any]` from the parsed JSON. */
  def getArray(array: Any): Seq[Any]

  def fromArray(array: Seq[Any]): Any

  def isScalar(any: Any): Boolean

  def getString(any: Any): Any
  def isString(any: Any): Boolean

  def convert(any: Any, oldAst: DataAst): Any
}

trait MutableDataAst extends DataAst {
  def setObjectValue(obj: Any, name: String, value: Any): Any
  def setArrayValue(array: Any, index: Int, value: Any): Any
  def removeObjectValue(obj: Any, name: String): Any
  def addArrayValue(array: Any, value: Any): Any
}
