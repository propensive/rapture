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

package rapture.json

import rapture.base._

import language.experimental.macros
abstract class DataContextMacros[+Data <: DataType[Data, JsonAst], -AstType <: JsonAst] {

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

private[json] object JsonDataMacros extends DataContextMacros[Json, JsonAst] {

  def dataCompanion(c: BlackboxContext): c.Expr[DataCompanion[Json, JsonAst]] = c.universe.reify(Json)

  def parseSource(s: List[String], stringsUsed: List[Boolean]) =
    try {
      JsonValidator.validate(s, stringsUsed)
      None
    } catch {
      case JsonValidator.ValidationException(strNo, pos, expected, found) =>
        val f = if (found == '\u0000') "end of input" else s"'$found'"
        Some((strNo, pos, s"failed to parse Json literal: expected $expected, but found $f"))
      case JsonValidator.DuplicateKeyException(strNo, pos, key) =>
        Some((strNo, pos, s"""duplicate key found in Json literal: "$key""""))
      case JsonValidator.NonStringKeyException(strNo, pos) =>
        Some((strNo, pos, s"""only Strings may be used as JSON object keys"""))
    }

  override def contextMacro(c: BlackboxContext)(exprs: c.Expr[ForcedConversion[Json]]*)(
      parser: c.Expr[Parser[String, JsonAst]]): c.Expr[Json] =
    super.contextMacro(c)(exprs: _*)(parser)

}

private[json] object JsonBufferDataMacros extends DataContextMacros[JsonBuffer, JsonBufferAst] {

  def dataCompanion(c: BlackboxContext): c.Expr[DataCompanion[JsonBuffer, JsonBufferAst]] =
    c.universe.reify(JsonBuffer)

  def parseSource(s: List[String], stringsUsed: List[Boolean]) =
    try {
      JsonValidator.validate(s, stringsUsed)
      None
    } catch {
      case JsonValidator.ValidationException(strNo, pos, expected, found) =>
        val f = if (found == '\u0000') "end of input" else s"'$found'"
        Some((strNo, pos, s"Failed to parse JsonBuffer literal: Expected $expected, but found $f."))
    }

  override def contextMacro(c: BlackboxContext)(exprs: c.Expr[ForcedConversion[JsonBuffer]]*)(
      parser: c.Expr[Parser[String, JsonBufferAst]]): c.Expr[JsonBuffer] =
    super.contextMacro(c)(exprs: _*)(parser)
}

object patternMatching {
  object exactArrays {
    implicit val implicitArrayMachingConfig = new ArrayMatchingConfig { def checkLengths = true }
  }

  object exactObjects {
    implicit val implicitArrayMachingConfig = new ObjectMatchingConfig { def checkSizes = true }
  }

  object exact {
    implicit val implicitArrayMachingConfig = new ObjectMatchingConfig with ArrayMatchingConfig {
      def checkSizes = true
      def checkLengths = true
    }
  }
}

object ArrayMatchingConfig {
  implicit val ignoreByDefault = new ArrayMatchingConfig { def checkLengths = false }
}

object ObjectMatchingConfig {
  implicit val ignoreByDefault = new ObjectMatchingConfig { def checkSizes = false }
}

trait ArrayMatchingConfig { def checkLengths: Boolean }
trait ObjectMatchingConfig { def checkSizes: Boolean }

class DataContext[+Data <: DataType[Data, JsonAst], -AstType <: JsonAst](companion: DataCompanion[Data, AstType],
                                                                         sc: StringContext) {

  protected def uniqueNonSubstring(s: String) = {
    var cur, m = 0
    s foreach { c =>
      cur = if (c == '_') cur + 1 else 0
      m = m max cur
    }
    "_" * (m + 1)
  }

  def unapplySeq[D <: DataType[D, JsonAst]](data: D)(
    implicit arrayMatching: ArrayMatchingConfig,
    objectMatching: ObjectMatchingConfig,
    parser: Parser[String, AstType]): Option[Seq[DataType[D, JsonAst]]] =
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


/** Provides support for JSON literals, in the form json" { } " or json""" { } """.
  * Interpolation is used to substitute variable names into the JSON, and to extract values
  * from a JSON string. */
private[json] class JsonStrings(sc: StringContext) {
  class JsonContext() extends DataContext(Json, sc) {
    def apply(exprs: ForcedConversion[Json]*)(implicit parser: Parser[String, JsonAst]): Json = macro JsonDataMacros.contextMacro
  }
  val json = new JsonContext()
}

private[json] class JsonBufferStrings(sc: StringContext) {
  class JsonBufferContext() extends DataContext(JsonBuffer, sc) {
    def apply(exprs: ForcedConversion[JsonBuffer]*)(implicit parser: Parser[String, JsonBufferAst]): JsonBuffer = macro JsonBufferDataMacros.contextMacro
  }
  val jsonBuffer = new JsonBufferContext()
}
