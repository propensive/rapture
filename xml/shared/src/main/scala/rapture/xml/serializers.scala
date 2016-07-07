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

import language.higherKinds

private[xml] case class DirectXmlSerializer[T](ast: XmlAst)

private[xml] trait Serializers {

  case class BasicXmlSerializer[T](serialization: T => Any) extends Serializer[T, Xml] {
    def serialize(t: T): Any = serialization(t)
  }

  implicit def xmlBufferSerializer[T](implicit ser: Serializer[T, Xml]): Serializer[T, XmlBuffer] =
    new Serializer[T, XmlBuffer] { def serialize(t: T): Any = ser.serialize(t) }

  implicit def intSerializer(implicit ast: XmlAst): Serializer[Int, Xml] =
    BasicXmlSerializer(ast fromString _.toString)

  implicit def booleanSerializer(implicit ast: XmlAst): Serializer[Boolean, Xml] =
    BasicXmlSerializer(ast fromString _.toString)

  implicit def stringSerializer(implicit ast: XmlAst): Serializer[String, Xml] =
    BasicXmlSerializer(ast.fromString)

  implicit def floatSerializer(implicit ast: XmlAst): Serializer[Float, Xml] =
    BasicXmlSerializer(ast fromString _.toString)

  implicit def doubleSerializer(implicit ast: XmlAst): Serializer[Double, Xml] =
    BasicXmlSerializer(ast fromString _.toString)

  implicit def bigDecimalSerializer(implicit ast: XmlAst): Serializer[BigDecimal, Xml] =
    BasicXmlSerializer(ast fromString _.toString)

  implicit def bigIntSerializer(implicit ast: XmlAst): Serializer[BigInt, Xml] =
    BasicXmlSerializer(ast fromString _.toString)

  implicit def longSerializer(implicit ast: XmlAst): Serializer[Long, Xml] =
    BasicXmlSerializer(ast fromString _.toString)

  implicit def shortSerializer(implicit ast: XmlAst): Serializer[Short, Xml] =
    BasicXmlSerializer(ast fromString _.toString)

  implicit def byteSerializer(implicit ast: XmlAst): Serializer[Byte, Xml] =
    BasicXmlSerializer(ast fromString _.toString)

  implicit def nilSerializer(implicit ast: XmlAst): Serializer[Nil.type, Xml] =
    BasicXmlSerializer(v => ast fromArray Nil)

  implicit def traversableSerializer[Type, Coll[T] <: Traversable[T]](
      implicit ast: XmlAst,
      ser: Serializer[Type, Xml]): Serializer[Coll[Type], Xml] =
    BasicXmlSerializer(ast fromArray _.map(ser.serialize).to[List])

  implicit def optionSerializer[Type](implicit ast: XmlAst,
                                      ser: Serializer[Type, Xml]): Serializer[Option[Type], Xml] =
    BasicXmlSerializer(_ map ser.serialize getOrElse ast.nullValue)

  implicit def mapSerializer[Type, Ast <: XmlAst](implicit ast: Ast,
                                                  ser: Serializer[Type, Xml]): Serializer[Map[String, Type], Xml] =
    new Serializer[Map[String, Type], Xml] {
      def serialize(m: Map[String, Type]) = ast.fromObject(m.mapValues(ser.serialize))
    }

  implicit def directXmlSerializer[T: DirectXmlSerializer](implicit ast: XmlAst): Serializer[T, Xml] =
    BasicXmlSerializer(
        obj => xmlSerializer.serialize(Xml.construct(MutableCell(obj), Vector())(?[DirectXmlSerializer[T]].ast)))

  implicit def xmlSerializer[XmlType <: XmlDataType[XmlType, _ <: XmlAst]](
      implicit ast: XmlAst): Serializer[XmlType, Xml] =
    BasicXmlSerializer[XmlType]({ j =>
      if (j.$ast == ast) j.$normalize else ast.convert(j.$normalize, j.$ast)
    })
}
