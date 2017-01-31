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

package rapture.xml.xmlBackends.stdlib

import rapture.xml._

import scala.xml._

private[stdlib] object StdlibAst extends XmlBufferAst {

  override def dereferenceObject(obj: Any, element: String): Any = obj match {
    case n: Node if n.child.exists(_.label == element) => n \ element
    case ns: NodeSeq if ns.exists(_.label == element) => ns.filter(_.label == element)
    case ns: NodeSeq if ns.exists(_.child.exists(_.label == element)) => ns \ element
    case _ => throw MissingValueException()
  }

  override def getChildren(obj: Any): Seq[Any] = obj match {
    case n: Node => n.child.to[List]
    case n: NodeSeq => n.flatMap(_.child).to[List]
    case _ => throw new Exception
  }

  def getArray(array: Any): List[Any] = array match {
    case ns: NodeSeq => ns.to[List]
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
  }

  def getString(string: Any): String = string match {
    case ns: NodeSeq => ns.text
    case _ => throw TypeMismatchException(getType(string), DataTypes.String)
  }

  def getObject(obj: Any): Map[String, Any] = obj match {
    case n: Node =>
      n.child.map { e =>
        e.label -> e.child
      }.toMap
    case n: NodeSeq =>
      n.flatMap(_.child.map { e =>
          e.label -> e.child
        })
        .toMap
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }

  def getLabel(obj: Any): String = obj match {
    case n: Node => n.label
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }

  def getAttributes(obj: Any): Map[String, String] = obj match {
    case n: Node => n.attributes.asAttrMap
    case v =>
      println(s"Found $v of type ${v.getClass}")
      throw TypeMismatchException(getType(obj), DataTypes.Object)
  }

  def setObjectValue(obj: Any, name: String, value: Any): Any =
    fromObject(getObject(obj).updated(name, value))

  def removeObjectValue(obj: Any, name: String): Any = obj match {
    case obj: Map[_, _] => obj.asInstanceOf[Map[String, Any]] - name
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object)
  }

  def addArrayValue(array: Any, value: Any): Any = array match {
    case array: NodeSeq => array :+ value
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
  }

  def setArrayValue(array: Any, index: Int, value: Any): Any = array match {
    case array: NodeSeq =>
      new NodeSeq {
        def theSeq = array.patch(index, List(value.asInstanceOf[Node]), 1).to[List]
      }
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array)
  }

  def isPi(any: Any): Boolean = any match {
    case ProcInstr(_, _) => true
    case _ => false
  }

  def getPiText(any: Any): String = any match {
    case ProcInstr(_, text) => text
    case _ => throw TypeMismatchException(getType(any), DataTypes.Object)
  }

  def isComment(any: Any): Boolean = any match {
    case Comment(_) => true
    case _ => false
  }

  def getComment(any: Any): String = any match {
    case Comment(text) => text
    case _ => throw TypeMismatchException(getType(any), DataTypes.Object)
  }

  def getPiTarget(any: Any): String = any match {
    case ProcInstr(target, _) => target
    case _ => throw TypeMismatchException(getType(any), DataTypes.Object)
  }

  def isArray(array: Any): Boolean = array match {
    case n: Node => false
    case ns: NodeSeq => true
    case _ => false
  }

  def isString(string: Any): Boolean = string match {
    case Text(_) => true
    case _ => false
  }

  def isObject(obj: Any): Boolean = obj match {
    case _: Node => true
    case _: NodeSeq => true
    case _ => false
  }

  def isNull(obj: Any): Boolean = false

  def fromArray(array: Seq[Any]): Any = array.collect { case e: NodeSeq => e }.foldLeft(NodeSeq.Empty)(_ ++ _)

  def fromObject(obj: Map[String, Any]): Any =
    obj
      .to[List]
      .collect {
        case (k, v: NodeSeq) =>
          Elem(null, k, Null, TopScope, true, v: _*): NodeSeq
      }
      .foldLeft(NodeSeq.Empty)(_ ++ _)

  def fromString(string: String): Any = Text(string)

  override val nullValue = ""

  override def toString = "<XmlStdlib>"

}
