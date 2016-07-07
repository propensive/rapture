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

package rapture.xml

import rapture.core._
import rapture.data._

import scala.util._

trait XmlAst extends DataAst {

  def isScalar(any: Any) = isString(any)

  def getScala(any: Any) = Try(getString(any)) getOrElse { throw new Exception }

  def getString(string: Any): String

  def fromString(string: String): Any

  def isComment(any: Any): Boolean

  def isPi(any: Any): Boolean

  def getPiText(any: Any): String

  def getComment(any: Any): String

  def getAttributes(obj: Any): Map[String, String]

  /** Tests if the element represents a `String` */
  def isString(any: Any): Boolean

  /** Returns the DataType instance for the particular type. */
  def getType(any: Any): DataTypes.DataType =
    if (isString(any)) DataTypes.String
    else if (isObject(any)) DataTypes.Object
    else if (isArray(any)) DataTypes.Array
    else throw MissingValueException()

  def convert(v: Any, ast: DataAst): Any = {
    val oldAst = ast.asInstanceOf[XmlAst]
    if (oldAst.isString(v)) fromString(oldAst.getString(v))
    else if (oldAst.isArray(v)) fromArray(oldAst.getArray(v).map(convert(_, oldAst)))
    else if (oldAst.isObject(v)) fromObject(oldAst.getObject(v).mapValues(convert(_, oldAst)))
    else nullValue
  }

  val nullValue = ""

  protected def typeTest(pf: PartialFunction[Any, Unit])(v: Any) = pf.isDefinedAt(v)
}

trait XmlBufferAst extends XmlAst with MutableDataAst
