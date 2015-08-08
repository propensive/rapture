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

import rapture.data._

import scala.xml._

package xmlBackends {
  package scalaXml {
    object `package` {
      implicit val scalaXmlAst = ScalaXmlAst
      implicit val scalaXmlBufferParser = ScalaXmlBufferParser
      
      implicit val scalaXmlNodeSerializer: DirectXmlSerializer[scala.xml.Node] =
        DirectXmlSerializer(ScalaXmlAst)
      
      implicit val scalaXmlElemSerializer: DirectXmlSerializer[scala.xml.Elem] =
        DirectXmlSerializer(ScalaXmlAst)
      
      implicit val scalaXmlNodeSeqSerializer: DirectXmlSerializer[scala.xml.NodeSeq] =
        DirectXmlSerializer(ScalaXmlAst)
    }
  }
}

/** The default XML AST implementation */
object ScalaXmlAst extends XmlBufferAst {
 
  override def dereferenceObject(obj: Any, element: String): Any = obj match {
    case ns: NodeSeq => ns \ element
  }

  def isObject(any: Any) = any match {
    case _: Node => true
    case _ => false
  }

  def isNull(any: Any) = false

  def getArray(array: Any): List[Any] = array match {
    case ns: NodeSeq => ns.to[List]
    case _ => throw TypeMismatchException(Some(getType(array) -> DataTypes.Array), Vector())
  }

  def fromArray(array: Seq[Any]): Any = array.collect{ case e: NodeSeq => e }.foldLeft(NodeSeq.Empty)(_ ++ _)

  def getString(string: Any): String = string match {
    case ns: NodeSeq => ns.text
    case _ => throw TypeMismatchException(Some(getType(string) -> DataTypes.String), Vector())
  }
  
  def fromString(string: String): Any = Text(string)
 
  override def getChildren(obj: Any): Seq[Any] = obj match {
    case n: Node => n.child.to[List]
    case _ => throw new Exception
  }

  def getObject(obj: Any): Map[String, Any] = obj match {
    case n: Node => n.child.map{ e => e.label -> e.child }.toMap
    case _ => throw TypeMismatchException(Some(getType(obj) -> DataTypes.Object), Vector())
  }
 
  def getLabel(obj: Any): String = obj match {
    case n: Node => n.label
    case _ => throw TypeMismatchException(Some(getType(obj) -> DataTypes.Object), Vector())
  }

  def getAttributes(obj: Any): Map[String, String] = obj match {
    case n: Node => n.attributes.asAttrMap
    case _ => throw TypeMismatchException(Some(getType(obj) -> DataTypes.Object), Vector())
  }

  def fromObject(obj: Map[String, Any]): Any = obj.to[List].collect{ case (k, v: NodeSeq) =>
    Elem(null, k, Null, TopScope, true, v: _*): NodeSeq
  }.foldLeft(NodeSeq.Empty)(_ ++ _)
  
  def setObjectValue(obj: Any, name: String, value: Any): Any =
    fromObject(getObject(obj).updated(name, value))
  
  def removeObjectValue(obj: Any, name: String): Any = obj match {
    case obj: Map[_, _] => obj.asInstanceOf[Map[String, Any]] - name
    case _ => throw TypeMismatchException(Some(getType(obj) -> DataTypes.Object), Vector())
  }
  
  def addArrayValue(array: Any, value: Any): Any = array match {
    case array: NodeSeq => array :+ value
    case _ => throw TypeMismatchException(Some(getType(array) -> DataTypes.Array), Vector())
  }
  
  def setArrayValue(array: Any, index: Int, value: Any): Any = array match {
    case array: NodeSeq =>
      new NodeSeq {
        def theSeq = array.patch(index, List(value.asInstanceOf[Node]), 1).to[List]
      }
    case _ => throw TypeMismatchException(Some(getType(array) -> DataTypes.Array), Vector())
  }
 
  def isPi(any: Any): Boolean = any match {
    case ProcInstr(_, _) => true
    case _ => false
  }

  def getPiText(any: Any): String = any match {
    case ProcInstr(_, text) => text
    case _ => throw TypeMismatchException(Some(getType(any) -> DataTypes.Object), Vector())
  }
  
  def isComment(any: Any): Boolean = any match {
    case Comment(_) => true
    case _ => false
  }
  
  def getComment(any: Any): String = any match {
    case Comment(text) => text
    case _ => throw TypeMismatchException(Some(getType(any) -> DataTypes.Object), Vector())
  }
  
  def getPiTarget(any: Any): String = any match {
    case ProcInstr(target, _) => target
    case _ => throw TypeMismatchException(Some(getType(any) -> DataTypes.Object), Vector())
  }
  
  def isString(any: Any): Boolean = any match {
    case Text(_) => true
    case _ => false
  }

  def isArray(any: Any): Boolean = any match {
    case n: Node => false
    case ns: NodeSeq => true
    case _ => false
  }
}

object ScalaXmlBufferParser extends Parser[String, XmlBufferAst] {
  val ast = ScalaXmlAst
  def parse(s: String): Option[Any] = try Some(XML.loadString(s)) catch { case e: Exception => None }
}

