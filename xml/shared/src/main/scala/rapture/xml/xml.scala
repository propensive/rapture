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

import language.experimental.macros
import scala.language.implicitConversions

private[xml] trait Xml_2 {
  implicit def xmlExtractorMacro[T <: Product, Th]: Extractor[T, Xml] = macro XmlMacros
    .xmlExtractorMacro[T, Th]

  implicit def xmlSerializerMacro[T <: Product](implicit ast: XmlAst): Serializer[T, Xml] = macro XmlMacros
    .xmlSerializerMacro[T]
}

private[xml] trait Xml_1 extends Xml_2 {
  implicit def dynamicWorkaround(j: Xml): DynamicWorkaround = new DynamicWorkaround(j)
}

private[xml] class DynamicWorkaround(xml: Xml) {
  def self: Xml = xml.selectDynamic("self")
}

trait `Xml.parse` extends MethodConstraint

private[xml] trait XmlDataCompanion[+Type <: XmlDataType[Type, AstType], AstType <: XmlAst]
    extends DataCompanion[Type, AstType] {

  type ParseMethodConstraint = `Xml.parse`

}

private[xml] object XmlDataType extends Extractors with Serializers

private[xml] trait XmlDataType[+T <: XmlDataType[T, AstType], AstType <: XmlAst] extends DataType[T, AstType]

object XmlBuffer extends XmlDataCompanion[XmlBuffer, XmlBufferAst] {

  def construct(any: MutableCell, path: Vector[Either[Int, String]])(implicit ast: XmlBufferAst): XmlBuffer =
    new XmlBuffer(any, path)
}

/** Companion object to the `Xml` type, providing factory and extractor methods, and a XML
  * pretty printer. */
object Xml extends XmlDataCompanion[Xml, XmlAst] with Xml_1 {
  
  def apply[T](t: T)(implicit ast: XmlAst, ser: Serializer[T, Xml]): Xml =
    construct(MutableCell(if(t == null) ast.nullValue else ser.serialize(t)), Vector())

  object DynamicApplied {
    implicit def auto[L](k: L)(implicit dynApp: DynamicApply[L]): DynamicApplied[L, dynApp.Result] =
      new DynamicApplied[L, dynApp.Result](dynApp, k) {
        def apply(v: Xml): dynApp.Result = dynApp(v, k)
      }
  }

  abstract class DynamicApplied[L, R](val dynApp: DynamicApply[L] { type Result = R }, key: L) {
    def apply(v: Xml): R
  }

  abstract class DynamicApply[L] {
    type Result
    def apply(v: Xml, k: L): Result
  }

  implicit val applyInt: DynamicApply[Int] { type Result = Xml } = new DynamicApply[Int] {
    type Result = Xml
    def apply(v: Xml, k: Int): Xml = v.$deref(Left(k) +: v.$path)
  }

  implicit val applySymbol: DynamicApply[Symbol] { type Result = XmlAttribute } = new DynamicApply[Symbol] {
    type Result = XmlAttribute
    def apply(v: Xml, k: Symbol): XmlAttribute = v.$attribute(k)
  }

  def construct(any: MutableCell, path: Vector[Either[Int, String]])(implicit ast: XmlAst): Xml = new Xml(any, path)

  def ast(xml: Xml): XmlAst = xml.$ast

  def extractor[T](implicit ext: Extractor[T, Xml]): Extractor[T, Xml] { type Throws = ext.Throws } = ext
  def serializer[T](implicit ser: Serializer[T, Xml]) = ser

  implicit def xmlCastExtractor[T: XmlCastExtractor](implicit ast: XmlAst): Extractor[T, XmlDataType[_, _ <: XmlAst]] =
    new Extractor[T, XmlDataType[_, _ <: XmlAst]] {
      type Throws = DataGetException
      def extract(value: XmlDataType[_, _ <: XmlAst],
                  ast2: DataAst,
                  mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, DataGetException] =
        mode.wrap(ast2 match {
          case ast2: XmlAst =>
            val norm = mode.catching[DataGetException, Any](value.$normalize)
            try {
              if (ast == ast2) norm.asInstanceOf[T]
              else
                XmlDataType.xmlSerializer.serialize(Xml.construct(MutableCell(norm), Vector())(ast2)).asInstanceOf[T]
            } catch {
              case e: ClassCastException =>
                mode.exception[T, DataGetException](
                    TypeMismatchException(ast.getType(norm), implicitly[XmlCastExtractor[T]].dataType))
            }
          case _ => ???
        })
    }

}

case class XmlAttribute(key: String, value: String) {
  def as[T: StringParser]: T = implicitly[StringParser[T]].parse(value, modes.throwExceptions())
  def is[T: StringParser]: Boolean =
    try {
      as[T]
      true
    } catch { case e: Exception => false }

  override def toString = s"""$key="$value""""
}

/** Represents some parsed XML. */
class Xml(val $root: MutableCell, val $path: Vector[Either[Int, String]] = Vector())(implicit val $ast: XmlAst)
    extends XmlDataType[Xml, XmlAst]
    with DynamicData[Xml, XmlAst] {

  def apply(attribute: Symbol): XmlAttribute = $attribute(attribute)
  def $attribute(attribute: Symbol): XmlAttribute =
    XmlAttribute(attribute.name, $ast.getAttributes($normalize)(attribute.name))
  def applyDynamic[L, R](field: String)(dynApp: Xml.DynamicApplied[L, R] = 0): R =
    dynApp($deref(Right(field) +: $path))

  def $wrap(any: Any, path: Vector[Either[Int, String]]): Xml =
    new Xml(MutableCell(any), path)

  def $deref(path: Vector[Either[Int, String]]): Xml = new Xml($root, path)

  def $extract(sp: Vector[Either[Int, String]]): Xml =
    if (sp.isEmpty) this
    else
      sp match {
        case Left(i) +: tail => apply(i).$extract(tail)
        case Right(e) +: tail => selectDynamic(e).$extract(tail)
      }

  override def toBareString =
    try Xml.format(this)(formatters.compact()($ast))
    catch {
      case e: Exception => "undefined"
    }

  override def toString: String = s"""xml""${'"'}$toBareString""${'"'}"""
}

class XmlBuffer(val $root: MutableCell, val $path: Vector[Either[Int, String]] = Vector())(
    implicit val $ast: XmlBufferAst)
    extends XmlDataType[XmlBuffer, XmlBufferAst]
    with MutableDataType[XmlBuffer, XmlBufferAst]
    with DynamicData[XmlBuffer, XmlBufferAst] {

  def $wrap(any: Any, path: Vector[Either[Int, String]]): XmlBuffer =
    new XmlBuffer(MutableCell(any), path)

  def $deref(path: Vector[Either[Int, String]]): XmlBuffer = new XmlBuffer($root, path)

  def $extract(sp: Vector[Either[Int, String]]): XmlBuffer =
    if (sp.isEmpty) this
    else
      sp match {
        case Left(i) +: tail => apply(i).$extract(tail)
        case Right(e) +: tail => selectDynamic(e).$extract(tail)
      }

  override def toBareString =
    try Xml.format(this)(formatters.compact()($ast))
    catch {
      case e: Exception => "undefined"
    }

  override def toString: String = s"""xml""${'"'}$toBareString""${'"'}"""
}
