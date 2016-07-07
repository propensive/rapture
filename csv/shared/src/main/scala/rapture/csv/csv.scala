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

package rapture.csv

import rapture.core._
import rapture.io._
import rapture.uri._
import rapture.fs._
import rapture.codec._

import encodings.`UTF-8`._

import language.experimental.macros

object ReadCsvHeader {
  implicit val readCsvHeader: ReadCsvHeader = ReadCsvHeader(true)
}
case class ReadCsvHeader(header: Boolean)

object CsvTest {
  import csvBackends.simple._
  def main(args: Seq[String]) = Csv.parse(uri"file:///home/jpretty/test.csv")
}

case class CsvParseException(line: Int, col: Int) extends RuntimeException {
  override def getMessage = s"""parse error: expected `"`"""
}
sealed trait CsvGetException extends RuntimeException
case class CsvMissingValue(col: Int) extends CsvGetException {
  override def getMessage = s"missing value: Column $col does not exist"
}
case class CsvTypeMismatch(typeName: String, col: Int) extends CsvGetException {
  override def getMessage = s"type mismatch: Could not read value of type $typeName in column $col"
}

trait `Csv.parse` extends MethodConstraint
trait `CsvRow#as` extends MethodConstraint
trait `Csv#mapAs` extends MethodConstraint
trait `CsvCell#as` extends MethodConstraint

class KeyedCsvRow(val csvRow: CsvRow, val keys: Map[String, Int]) {
  def apply(key: String): CsvCell = csvRow.apply(keys(key))
}

class CsvRow(val elems: Seq[String]) {
  def apply(idx: Int): CsvCell = CsvCell(elems(idx), idx)
  def as[T](implicit mode: Mode[`CsvRow#as`], extractor: CsvRowExtractor[T]): mode.Wrap[T, CsvGetException] =
    extractor.extract(elems, mode)

  override def toString = elems.mkString("\"", "\",\"", "\"")
}

object CsvRowExtractor {
  implicit def generateExtractor[T]: CsvRowExtractor[T] = macro Macros.extractorMacro[T]
}
trait CsvRowExtractor[T] {
  def extract(values: Seq[String], mode: Mode[_]): mode.Wrap[T, CsvGetException]
}

case class BadLength(len: Int) extends Exception("Bad length: " + len)

object CsvCellExtractor {
  implicit val stringExtractor: CsvCellExtractor[String] = new CsvCellExtractor[String] {
    type ExceptionType = CsvGetException
    def extract(value: String, c: Int, mode: Mode[_]): mode.Wrap[String, CsvGetException] =
      mode.wrap(value)
  }

  implicit val intExtractor: CsvCellExtractor[Int] = new CsvCellExtractor[Int] {
    type ExceptionType = CsvGetException
    def extract(value: String, c: Int, mode: Mode[_]): mode.Wrap[Int, CsvGetException] =
      mode.wrap {
        try value.toInt
        catch { case e: Exception => mode.exception(CsvTypeMismatch("integer", c)) }
      }
  }

  implicit val doubleExtractor: CsvCellExtractor[Double] = new CsvCellExtractor[Double] {
    type ExceptionType = CsvGetException
    def extract(value: String, c: Int, mode: Mode[_]): mode.Wrap[Double, CsvGetException] =
      mode.wrap {
        try value.toDouble
        catch { case e: Exception => mode.exception(CsvTypeMismatch("double", c)) }
      }
  }

  implicit val charExtractor: CsvCellExtractor[Char] = new CsvCellExtractor[Char] {
    type ExceptionType = BadLength
    def extract(value: String, c: Int, mode: Mode[_]): mode.Wrap[Char, CsvGetException with BadLength] = mode.wrap {
      if (value.length == 1) value.head
      else {
        mode.exception(CsvTypeMismatch("character", c))
        '\u0000'
      }
    }
  }
}

trait CsvCellExtractor[T] {
  type ExceptionType <: Exception
  def extract(value: String, c: Int, mode: Mode[_]): mode.Wrap[T, CsvGetException with ExceptionType]
}

case class CsvCell(value: String, col: Int) {
  def as[T](mode: Mode[`CsvCell#as`])(ext: CsvCellExtractor[T]): mode.Wrap[T, CsvGetException with ext.ExceptionType] =
    ext.extract(value, col, mode)

  override def toString = value
}

case class Csv(rows: Vector[CsvRow]) extends Seq[CsvRow] {
  def iterator = rows.iterator
  def apply(idx: Int) = rows(idx)
  def length = rows.length

  override def toString = rows.take(4).mkString("\n") + (if (rows.length > 4) "\n..." else "")

  def mapAs[T: CsvRowExtractor](implicit mode: Mode[`Csv#mapAs`]): mode.Wrap[Seq[T], CsvGetException] = mode.wrap {
    rows.map { row =>
      mode.unwrap(?[CsvRowExtractor[T]].extract(row.elems, mode))
    }
  }
}

object Csv {
  def parse[Res](resource: Res)(implicit mode: Mode[`Csv.parse`],
                                reader: Reader[Res, String],
                                backend: CsvBackend): mode.Wrap[Csv, CsvParseException] = mode.wrap {
    Csv(reader.input(resource).to[Vector].zipWithIndex.map {
      case (ln, idx) =>
        new CsvRow(mode.unwrap(backend.parseRow(ln, idx, mode)))
    })
  }

  def extractor[T: CsvCellExtractor] = ?[CsvCellExtractor[T]]
}

trait CsvBackend { def parseRow(s: String, line: Int, mode: Mode[_]): mode.Wrap[Seq[String], CsvParseException] }

object csvBackends {
  object simple {
    implicit val simpleBackend: CsvBackend = new CsvBackend {
      def parseRow(s: String, line: Int, mode: Mode[_]): mode.Wrap[Seq[String], CsvParseException] = mode.wrap {
        var cells: Vector[String] = Vector()
        var cur = 0
        var quoted = false
        var expectingQuote = false
        var sb = new StringBuilder
        while (cur < s.length) {
          s(cur) match {
            case '"' =>
              if (sb.isEmpty && !quoted) {
                quoted = true
              } else if (quoted) {
                quoted = false
              } else if (expectingQuote) {
                sb.append('"')
                expectingQuote = false
              } else expectingQuote = true
              cur += 1
            case _ if expectingQuote =>
              mode.exception(CsvParseException(line, cells.length))
            case ',' if !quoted =>
              cells = cells :+ sb.toString.trim
              sb = new StringBuilder
              cur += 1
            case other =>
              sb.append(other)
              cur += 1
          }
        }
        cells :+ sb.toString.trim
      }
    }
  }
}
