/******************************************************************************************************************\
* Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.                                          *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance   *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.core

import scala.util.Try

object ParseException
case class ParseException(bad: String, typ: String) extends Exception(s"could not parse '$bad' as $typ")

package booleanParsing {
  object strict {
    def apply() = implicitBooleanParsing
    implicit def implicitBooleanParsing(implicit br: BooleanRepresentation): BooleanParser =
      new BooleanParser {
        def parse(s: String, mode: Mode[_]): mode.Wrap[Boolean, InvalidBoolean] = mode.wrap {
          if(s == br.trueValue) true
          else if(s == br.falseValue) false
          else mode.exception(InvalidBoolean(s))
        }
      }
  }

  object permissive {
    def apply(): BooleanParser = implicitBooleanParsing
    private val trueValues = List("true", "yes", "on", "1")
    private val falseValues = List("false", "no", "off", "0")
    implicit val implicitBooleanParsing: BooleanParser = new BooleanParser {
      def parse(b: String, mode: Mode[_]): mode.Wrap[Boolean, InvalidBoolean] = mode.wrap {
        if(trueValues.contains(b.toLowerCase)) true
        else if(falseValues.contains(b.toLowerCase)) false
        else mode.exception(ParseException(b, "boolean using permissive parser"))
      }
    }
  }
}

object BooleanParser { implicit val implicitBooleanParser: BooleanParser = booleanParsing.permissive() }
trait BooleanParser { def parse(s: String, mode: Mode[_]): mode.Wrap[Boolean, InvalidBoolean] }

abstract class StringParser[T] extends Functor[StringParser, T] { strp =>
  type Throws <: Exception
  def parse(string: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, Throws]

  def rawMap[T2](fn: (T, Mode[_ <: MethodConstraint]) => T2): StringParser[T2] { type Throws = strp.Throws } =
    new StringParser[T2] {
      type Throws = strp.Throws
      def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T2, Throws] =
        mode.wrap(fn(mode.unwrap(strp.parse(s, mode)), mode))
    }
}

case class InvalidBoolean(value: String) extends Exception(s"""The value "$value" is not a valid boolean.""")
case class InvalidNumber(value: String, numberType: String) extends Exception(s"""The value "$value" is not a valid $numberType.""")

trait StringParser_1 {
  implicit def optParser[T: StringParser]: StringParser[Option[T]] { type Throws = Nothing } = new StringParser[Option[T]] {
    type Throws = Nothing
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Option[T], Nothing] = mode.wrap {
      try Some(mode.unwrap(?[StringParser[T]].parse(s, mode))) catch {
        case e: Exception => None
      }
    }
  }
  
  implicit def tryParser[T: StringParser]: StringParser[Try[T]] { type Throws = Nothing } = new StringParser[Try[T]] {
    type Throws = Nothing
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Try[T], Nothing] = mode.wrap {
      ?[StringParser[T]].parse(s, modes.returnTry())
    }
  }
}

object StringParser extends StringParser_1 {
  implicit def booleanParser(implicit bp: BooleanParser): StringParser[Boolean] { type Throws = InvalidBoolean } = new StringParser[Boolean] {
    type Throws = InvalidBoolean
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Boolean, InvalidBoolean] = bp.parse(s, mode.generic)
  }

  implicit val byteParser: StringParser[Byte] { type Throws = InvalidNumber } = new StringParser[Byte] {
    type Throws = InvalidNumber
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Byte, InvalidNumber] = mode.wrap {
      try java.lang.Byte.parseByte(s) catch {
        case e: NumberFormatException => mode.exception(InvalidNumber(s, "byte"))
      }
    }
  }

  implicit val charParser: StringParser[Char] { type Throws = InvalidNumber } = new StringParser[Char] {
    type Throws = InvalidNumber
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Char, InvalidNumber] = mode.wrap {
      if(s.length == 1) s.charAt(0) else mode.exception(InvalidNumber(s, "character"))
    }
  }

  implicit val shortParser: StringParser[Short] { type Throws = InvalidNumber } = new StringParser[Short] {
    type Throws = InvalidNumber
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Short, InvalidNumber] = mode.wrap {
      try java.lang.Short.parseShort(s) catch {
        case e: NumberFormatException => mode.exception(InvalidNumber(s, "short"))
      }
    }
  }

  implicit val intParser: StringParser[Int] { type Throws = InvalidNumber } = new StringParser[Int] {
    type Throws = InvalidNumber
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Int, InvalidNumber] = mode.wrap {
      try java.lang.Integer.parseInt(s) catch {
        case e: NumberFormatException => mode.exception(InvalidNumber(s, "integer"))
      }
    }
  }

  implicit val longParser: StringParser[Long] { type Throws = InvalidNumber } = new StringParser[Long] {
    type Throws = InvalidNumber
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Long, InvalidNumber] = mode.wrap {
      try java.lang.Long.parseLong(s) catch {
        case e: NumberFormatException => mode.exception(InvalidNumber(s, "long"))
      }
    }
  }

  implicit val stringParser: StringParser[String] { type Throws = Nothing } = new StringParser[String] {
    type Throws = Nothing
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[String, Nothing] = mode.wrap(s)
  }

  implicit val doubleParser: StringParser[Double] = new StringParser[Double] {
    type Throws = InvalidNumber
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Double, InvalidNumber] = mode.wrap {
      try java.lang.Double.parseDouble(s) catch {
        case e: NumberFormatException => mode.exception(ParseException(s, "double"))
      }
    }
  }

  implicit val floatParser: StringParser[Float] = new StringParser[Float] {
    type Throws = InvalidNumber
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Float, InvalidNumber] = mode.wrap {
      try java.lang.Float.parseFloat(s) catch {
        case e: NumberFormatException => mode.exception(InvalidNumber(s, "float"))
      }
    }
  }
}

