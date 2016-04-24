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

import rapture.base._
import rapture.data._

private[xml] object XmlMacros {
  def xmlExtractorMacro[T: c.WeakTypeTag, Th](c: WhiteboxContext): c.Expr[Extractor[T, Xml] { type Throws = Th }] =
    Macros.extractorMacro[T, Xml, Th](c)
  
  //def xmlBufferExtractorMacro[T: c.WeakTypeTag](c: Context) =
  //  Macros.extractorMacro2[T, XmlBuffer](c)
  
  def xmlSerializerMacro[T: c.WeakTypeTag](c: WhiteboxContext)(ast: c.Expr[XmlAst]):
      c.Expr[Serializer[T, Xml]] =
    Macros.serializerMacro[T, Xml](c)(ast)
  
  def xmlBufferSerializerMacro[T: c.WeakTypeTag](c: WhiteboxContext)(ast: c.Expr[XmlBufferAst]):
      c.Expr[Serializer[T, XmlBuffer]] =
    Macros.serializerMacro[T, XmlBuffer](c)(ast)
}
