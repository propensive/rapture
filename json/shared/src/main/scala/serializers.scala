/******************************************************************************************************************\
* Rapture JSON, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                     *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in complance    *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.json

import rapture.core._
import rapture.data._

import scala.reflect.macros._
import scala.annotation._

import language.experimental.macros
import language.higherKinds

private[json] case class DirectJsonSerializer[T](ast: JsonAst)

private[json] trait Serializers {

  case class BasicJsonSerializer[T](serialization: T => Any)
      extends Serializer[T, Json] { def serialize(t: T): Any = serialization(t) }

  implicit def jsonBufferSerializer[T](implicit ser: Serializer[T, Json]):
      Serializer[T, JsonBuffer] =
    new Serializer[T, JsonBuffer] { def serialize(t: T): Any = ser.serialize(t) }

  implicit def intSerializer(implicit ast: JsonAst): Serializer[Int, Json] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def booleanSerializer(implicit ast: JsonAst): Serializer[Boolean, Json] =
    BasicJsonSerializer(ast fromBoolean _)

  implicit def stringSerializer(implicit ast: JsonAst): Serializer[String, Json] =
    BasicJsonSerializer(ast fromString _)

  implicit def floatSerializer(implicit ast: JsonAst): Serializer[Float, Json] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def doubleSerializer(implicit ast: JsonAst): Serializer[Double, Json] =
    BasicJsonSerializer(ast fromDouble _)

  implicit def bigDecimalSerializer(implicit ast: JsonAst): Serializer[BigDecimal, Json] =
    BasicJsonSerializer(ast fromBigDecimal _)

  implicit def bigIntSerializer(implicit ast: JsonAst): Serializer[BigInt, Json] =
    BasicJsonSerializer(ast fromBigDecimal BigDecimal(_))

  implicit def longSerializer(implicit ast: JsonAst): Serializer[Long, Json] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def shortSerializer(implicit ast: JsonAst): Serializer[Short, Json] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def byteSerializer(implicit ast: JsonAst): Serializer[Byte, Json] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def nilSerializer(implicit ast: JsonAst): Serializer[Nil.type, Json] =
    BasicJsonSerializer(v => ast fromArray Nil)

  implicit def traversableSerializer[Type, Coll[T] <: Traversable[T]]
      (implicit ast: JsonAst, ser: Serializer[Type, Json]): Serializer[Coll[Type], Json] =
    BasicJsonSerializer(ast fromArray _.map(ser.serialize).to[List])

  implicit def optionSerializer[Type]
      (implicit ast: JsonAst, ser: Serializer[Type, Json]): Serializer[Option[Type], Json] =
    BasicJsonSerializer(_ map ser.serialize getOrElse ast.nullValue)

  implicit def mapSerializer[Type, Ast <: JsonAst]
      (implicit ast: Ast, ser: Serializer[Type, Json]): Serializer[Map[String, Type], Json] =
    new Serializer[Map[String, Type], Json] {
      def serialize(m: Map[String, Type]) = ast.fromObject(m.mapValues(ser.serialize))
    }

  implicit def directJsonSerializer[T: DirectJsonSerializer](implicit ast: JsonAst):
      Serializer[T, Json] =
    BasicJsonSerializer(obj => jsonSerializer.serialize(Json.construct(MutableCell(obj),
        Vector())(?[DirectJsonSerializer[T]].ast)))

  implicit def jsonSerializer[JsonType <: JsonDataType[JsonType, _ <: JsonAst]]
      (implicit ast: JsonAst): Serializer[JsonType, Json] =
    BasicJsonSerializer[JsonType]({ j =>
      if(j.$ast == ast) j.$normalize else {
        val oldAst = j.$ast

        def convert(v: Any): Any =
          if(oldAst.isString(v)) ast.fromString(oldAst.getString(v))
          else if(oldAst.isBoolean(v)) ast.fromBoolean(oldAst.getBoolean(v))
          else if(oldAst.isNumber(v)) ast.fromDouble(oldAst.getDouble(v))
          else if(oldAst.isArray(v)) ast.fromArray(oldAst.getArray(v).map(convert))
          else if(oldAst.isObject(v)) ast.fromObject(oldAst.getObject(v).mapValues(convert))
          else ast.nullValue

        convert(j.$normalize) // FIXME: Use custom mode
      }
    })
}
