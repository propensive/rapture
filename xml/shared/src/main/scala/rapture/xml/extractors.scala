/******************************************************************************************************************\
* Rapture XML, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                      *
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
package rapture.xml

import rapture.core._
import rapture.data._

import scala.util._

import language.higherKinds

private[xml] case class XmlCastExtractor[T](ast: XmlAst, dataType: DataTypes.DataType)

private[xml] trait Extractors extends Extractors_1 {

  implicit def optionExtractor[T](implicit ext: Extractor[T, Xml]): Extractor[Option[T], Xml] { type Throws =
      Nothing } = GeneralExtractors.optionExtractor[Xml, T]

  implicit def tryExtractor[T](implicit ext: Extractor[T, Xml]): Extractor[Try[T], Xml] { type Throws =
      Nothing } = GeneralExtractors.tryExtractor[Xml, T]

  implicit def genSeqExtractor[T, Coll[_]](implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]],
      ext: Extractor[T, Xml]): Extractor[Coll[T], Xml] { type Throws = ext.Throws } = {
    
    GeneralExtractors.genSeqExtractor[T, Coll, Xml]
  }
  
  implicit def xmlExtractor(implicit ast: XmlAst): Extractor[Xml, Xml] { type Throws = DataGetException } =
    new Extractor[Xml, Xml] {
      type Throws = DataGetException
      def extract(any: Xml, dataAst: DataAst, mode: Mode[_]): mode.Wrap[Xml, DataGetException] =
        mode.wrap(mode.catching[DataGetException, Xml](any.$wrap(any.$normalize)))
    }
 
}

private[xml] trait Extractors_1 {
  
  implicit def stringableExtractors[T](implicit ext: StringParser[T]): Extractor[T, Xml] {
      type Throws = DataGetException with ext.Throws } = new Extractor[T, Xml] {
    
    type Throws = DataGetException with ext.Throws

    def extract(any: Xml, ast: DataAst, mode: Mode[_]): mode.Wrap[T, DataGetException with ext.Throws] = mode.wrap {
      val value: String = mode.catching[DataGetException, String](any.$ast.getString(any.$normalize))
      mode.unwrap(ext.parse(value, mode))
    }
  }
  
  implicit def xmlBufferExtractor[T](implicit xmlAst: XmlAst, ext: Extractor[T, Xml]):
      Extractor[T, XmlBuffer] { type Throws = ext.Throws } = new Extractor[T, XmlBuffer] {
    type Throws = ext.Throws
    def extract(any: XmlBuffer, ast: DataAst, mode: Mode[_]): mode.Wrap[T, ext.Throws] =
      ext.extract(Xml.construct(MutableCell(any.$root.value), Vector()), ast, mode)
  }
  
  implicit def xmlBufferToXmlExtractor(implicit ast: XmlBufferAst): Extractor[XmlBuffer, Xml] =
    new Extractor[XmlBuffer, Xml] {
      type Throws = DataGetException
      def extract(any: Xml, dataAst: DataAst, mode: Mode[_]): mode.Wrap[XmlBuffer, Throws] =
        mode.wrap(XmlBuffer.construct(MutableCell(XmlDataType.xmlSerializer.serialize(any)), Vector()))
    }

}
