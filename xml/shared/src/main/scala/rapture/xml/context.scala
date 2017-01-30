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
import rapture.core._

import language.experimental.macros
object ArrayMatchingConfig {
  implicit val ignoreByDefault = new ArrayMatchingConfig { def checkLengths = false }
}

object ObjectMatchingConfig {
  implicit val ignoreByDefault = new ObjectMatchingConfig { def checkSizes = false }
}

trait ArrayMatchingConfig { def checkLengths: Boolean }
trait ObjectMatchingConfig { def checkSizes: Boolean }

abstract class DataContextMacros[+Data <: DataType[Data, XmlAst], -AstType <: XmlAst] {

  def parseSource(s: List[String], stringsUsed: List[Boolean]): Option[(Int, Int, String)]

  def dataCompanion(c: BlackboxContext): c.Expr[DataCompanion[Data, AstType]]

  def contextMacro(c: BlackboxContext)(exprs: c.Expr[ForcedConversion[Data]]*)(
    parser: c.Expr[Parser[String, AstType]]): c.Expr[Data] = {
    import c.universe._

    c.prefix.tree match {
      case Select(Apply(Apply(_, List(Apply(_, rawParts))), _), _) =>
        val ys = rawParts.to[List]
        val text = rawParts map { case lit @ Literal(Constant(part: String)) => part }

        val listExprs = c.Expr[List[ForcedConversion[Data]]](q"_root_.scala.List(..${exprs.map(_.tree).to[List]})")

        val stringsUsed: List[Boolean] = listExprs.tree match {
          case Apply(_, bs) =>
            bs.map {
              case Apply(Apply(TypeApply(Select(_, nme), _), _), _) => nme.toString == "forceStringConversion"
            }
        }

        parseSource(text, stringsUsed) foreach {
          case (n, offset, msg) =>
            val oldPos = ys(n).asInstanceOf[Literal].pos

            val newPos = oldPos.withPoint(oldPos.start + offset)
            c.error(newPos, msg)
        }

        val listParts = c.Expr[List[ForcedConversion[Data]]](q"_root_.scala.List(..$rawParts)")

        val comp = dataCompanion(c)

        reify {
          val sb = new StringBuilder
          val textParts = listParts.splice.iterator
          val expressions: Iterator[ForcedConversion[_]] = listExprs.splice.iterator

          sb.append(textParts.next())

          while (textParts.hasNext) {
            sb.append(
              comp.splice.construct(MutableCell(expressions.next.value), Vector())(parser.splice.ast).toBareString)
            sb.append(textParts.next)
          }

          comp.splice.construct(MutableCell(parser.splice.parse(sb.toString).get), Vector())(parser.splice.ast)
        }
    }
  }
}

class DataContext[+Data <: DataType[Data, XmlAst], -AstType <: XmlAst](companion: DataCompanion[Data, AstType],
                                                                         sc: StringContext) {

  protected def uniqueNonSubstring(s: String) = {
    var cur, m = 0
    s foreach { c =>
      cur = if (c == '_') cur + 1 else 0
      m = m max cur
    }
    "_" * (m + 1)
  }

  def unapplySeq[D <: DataType[D, XmlAst]](data: D)(
    implicit arrayMatching: ArrayMatchingConfig,
    objectMatching: ObjectMatchingConfig,
    parser: Parser[String, AstType]): Option[Seq[DataType[D, XmlAst]]] =
    try {
      val placeholder = uniqueNonSubstring(sc.parts.mkString)
      val PlaceholderNumber = (placeholder + "([0-9]+)" + placeholder).r
      val count = Iterator.from(0)
      val txt = sc.parts.reduceLeft(_ + s""""${placeholder}${count.next()}${placeholder}" """ + _)

      val paths: Array[Vector[Either[Int, String]]] =
        Array.fill[Vector[Either[Int, String]]](sc.parts.length - 1)(Vector())

      val arrayLengths = new collection.mutable.HashMap[Vector[Either[Int, String]], Int]
      val objectSizes = new collection.mutable.HashMap[Vector[Either[Int, String]], Int]

      def extract(any: Any, path: Vector[Either[Int, String]]): Unit = {
        // FIXME: Avoid using isScalar if possible
        if (parser.ast.isScalar(any)) {
          val ext = data.$extract(path).as[Any]
          if (data.$extract(path).as[Any] != any) throw new Exception("Value doesn't match (1)")
        } else if (parser.ast.isObject(any)) {
          val obj = parser.ast.getObject(any)
          objectSizes(path) = obj.size
          obj foreach {
            case (k, v) =>
              if (parser.ast.isString(v)) parser.ast.getString(v) match {
                case str: CharSequence =>
                  str match {
                    case PlaceholderNumber(n) =>
                      paths(n.toInt) = path :+ Right(k)
                    case _ => extract(v, path :+ Right(k))
                  }
              } else extract(v, path :+ Right(k))
          }
        } else if (parser.ast.isArray(any)) {
          val array = parser.ast.getArray(any)
          if (arrayMatching.checkLengths) arrayLengths(path) = array.length
          array.zipWithIndex foreach {
            case (e, i) =>
              if (parser.ast.isString(e)) parser.ast.getString(e) match {
                case str: CharSequence =>
                  str match {
                    case PlaceholderNumber(n) =>
                      paths(n.toInt) = path :+ Left(i)
                    case _ => extract(e, path :+ Left(i))
                  }
              } else extract(e, path :+ Left(i))
          }
        } else throw new Exception("Value doesn't match (2)")
      }

      extract(parser.parse(txt).get, Vector())

      // Using a ListBuffer to work around SI-8947
      val extractsBuffer = new collection.mutable.ListBuffer[D]
      paths foreach { p =>
        extractsBuffer += data.$extract(p)
      }
      val extracts = extractsBuffer.toList
      extracts.foreach(_.$normalize)
      val matchedArrayLengths = arrayLengths.forall {
        case (p, len) =>
          parser.ast.getArray(data.$extract(p).$normalize).length == len
      }

      val matchedObjectSizes = objectSizes.forall {
        case (p, s) =>
          if (objectMatching.checkSizes) parser.ast.getObject(data.$extract(p).$normalize).size == s
          else parser.ast.getObject(data.$extract(p).$normalize).size >= 0
      }

      if (extracts.exists(_.$root.value == null) || !matchedArrayLengths || !matchedObjectSizes)
        None
      else Some(extracts)
    } catch {
      case e: Exception =>
        None
    }
}


private[xml] object XmlDataMacros extends DataContextMacros[Xml, XmlAst] {

  def dataCompanion(c: BlackboxContext): c.Expr[DataCompanion[Xml, XmlAst]] = c.universe.reify(Xml)

  def parseSource(s: List[String], stringsUsed: List[Boolean]) =
    try {
      XmlValidator.validate(s)
      None
    } catch {
      case XmlValidator.ValidationException(strNo, pos, expected, found) =>
        val f = if (found == '\u0000') "end of input" else s"'$found'"
        Some((strNo, pos, s"failed to parse Xml literal: expected $expected, but found $f"))
      case XmlValidator.DuplicateKeyException(strNo, pos, key) =>
        Some((strNo, pos, s"""duplicate key found in Xml literal: "$key""""))
    }

  override def contextMacro(c: BlackboxContext)(exprs: c.Expr[ForcedConversion[Xml]]*)(
      parser: c.Expr[Parser[String, XmlAst]]): c.Expr[Xml] =
    super.contextMacro(c)(exprs: _*)(parser)

}

private[xml] object XmlBufferDataMacros extends DataContextMacros[XmlBuffer, XmlBufferAst] {

  def dataCompanion(c: BlackboxContext): c.Expr[DataCompanion[XmlBuffer, XmlBufferAst]] =
    c.universe.reify(XmlBuffer)

  def parseSource(s: List[String], stringsUsed: List[Boolean]) =
    try {
      XmlValidator.validate(s)
      None
    } catch {
      case XmlValidator.ValidationException(strNo, pos, expected, found) =>
        val f = if (found == '\u0000') "end of input" else s"'$found'"
        Some((strNo, pos, s"Failed to parse XmlBuffer literal: Expected $expected, but found $f."))
    }

  override def contextMacro(c: BlackboxContext)(exprs: c.Expr[ForcedConversion[XmlBuffer]]*)(
      parser: c.Expr[Parser[String, XmlBufferAst]]): c.Expr[XmlBuffer] =
    super.contextMacro(c)(exprs: _*)(parser)
}

/** Provides support for XML literals, in the form xml" { } " or xml""" { } """.
  * Interpolation is used to substitute variable names into the XML, and to extract values
  * from a XML string. */
private[xml] class XmlStrings(sc: StringContext) {
  class XmlContext() extends DataContext(Xml, sc) {
    def apply(exprs: ForcedConversion[Xml]*)(implicit parser: Parser[String, XmlAst]): Xml = macro XmlDataMacros.contextMacro
  }
  val xml = new XmlContext()
}

private[xml] class XmlBufferStrings(sc: StringContext) {
  class XmlBufferContext() extends DataContext(XmlBuffer, sc) {
    def apply(exprs: ForcedConversion[XmlBuffer]*)(implicit parser: Parser[String, XmlBufferAst]): XmlBuffer = macro XmlBufferDataMacros.contextMacro
  }
  val xmlBuffer = new XmlBufferContext()
}
