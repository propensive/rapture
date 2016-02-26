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

package decimalFormats {
  object to0dp {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = DecimalPlaces(0)
  }
  object to1dp {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = DecimalPlaces(1)
  }
  object to2dp {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = DecimalPlaces(2)
  }
  object to3dp {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = DecimalPlaces(3)
  }
  object to4dp {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = DecimalPlaces(4)
  }
  object to5dp {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = DecimalPlaces(5)
  }
  object to6df {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = DecimalPlaces(6)
  }
  object to1sf {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = SignificantFigures(1)
  }
  object to2sf {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = SignificantFigures(2)
  }
  object to3sf {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = SignificantFigures(3)
  }
  object to4sf {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = SignificantFigures(4)
  }
  object to5sf {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = SignificantFigures(5)
  }
  object to6sf {
    def apply() = implicitDecimalFormat
    implicit val implicitDecimalFormat: DecimalFormat = SignificantFigures(6)
  }
}

package integerFormats {
  object exact {
    def apply() = implicitIntegerFormat
    implicit val implicitIntegerFormat: IntegerFormat = ExactInteger
  }
  object to1sf {
    def apply() = implicitIntegerFormat
    implicit val implicitIntegerFormat: IntegerFormat = IntegerSignificantFigures(1)
  }
  object to2sf {
    def apply() = implicitIntegerFormat
    implicit val implicitIntegerFormat: IntegerFormat = IntegerSignificantFigures(2)
  }
  object to3sf {
    def apply() = implicitIntegerFormat
    implicit val implicitIntegerFormat: IntegerFormat = IntegerSignificantFigures(3)
  }
  object to4sf {
    def apply() = implicitIntegerFormat
    implicit val implicitIntegerFormat: IntegerFormat = IntegerSignificantFigures(4)
  }
  object to5sf {
    def apply() = implicitIntegerFormat
    implicit val implicitIntegerFormat: IntegerFormat = IntegerSignificantFigures(5)
  }
  object to6sf {
    def apply() = implicitIntegerFormat
    implicit val implicitIntegerFormat: IntegerFormat = IntegerSignificantFigures(6)
  }
}

package booleanRepresentations {
  object trueFalse {
    def apply() = implicitBooleanRepresentation
    implicit val implicitBooleanRepresentation: BooleanRepresentation = BooleanRepresentation("true", "false")
  }
  
  object digital {
    def apply() = implicitBooleanRepresentation
    implicit val implicitBooleanRepresentation: BooleanRepresentation = BooleanRepresentation("1", "0")
  }

  object yesNo {
    def apply() = implicitBooleanRepresentation
    implicit val implicitBooleanRepresentation: BooleanRepresentation = BooleanRepresentation("yes", "no")
  }
  
  object onOff {
    def apply() = implicitBooleanRepresentation
    implicit val implicitBooleanRepresentation: BooleanRepresentation = BooleanRepresentation("on", "off")
  }
}

object BooleanRepresentation {
  implicit val defaultBooleanRepresentation: BooleanRepresentation = BooleanRepresentation("true", "false")
}

case class BooleanRepresentation(trueValue: String, falseValue: String)

object DecimalFormat { implicit val defaultRounding: DecimalFormat = SignificantFigures(4) }
trait DecimalFormat { def format(bigDecimal: BigDecimal): String }

case class DecimalPlaces(n: Int) extends DecimalFormat {
  def format(bigDecimal: BigDecimal): String = {
    val integral = bigDecimal.toBigInt.toString.length
    bigDecimal.round(new java.math.MathContext(integral + n)).setScale(n).toString
  }
}

case class SignificantFigures(n: Int) extends DecimalFormat {
  def format(bigDecimal: BigDecimal) = bigDecimal.round(new java.math.MathContext(n)).toString
}

object IntegerFormat { implicit val defaultRounding: IntegerFormat = ExactInteger }
trait IntegerFormat { def format(bigInt: BigInt): String }

case object ExactInteger extends IntegerFormat {
  def format(bigInt: BigInt) = bigInt.toString
}
case class IntegerSignificantFigures(n: Int) extends IntegerFormat {
  def format(bigInt: BigInt) =
    BigDecimal(bigInt).round(new java.math.MathContext(n)).toString
}

object StringSerializer {
  implicit def booleanSerializer(implicit bs: BooleanRepresentation): StringSerializer[Boolean] =
    new StringSerializer[Boolean] {
      def serialize(s: Boolean): String = if(s) bs.trueValue else bs.falseValue
    }

  implicit val charSerializer: StringSerializer[Char] = new StringSerializer[Char] {
    def serialize(s: Char): String = s.toString
  }

  implicit def byteSerializer(implicit df: IntegerFormat): StringSerializer[Byte] =
    new StringSerializer[Byte] { def serialize(s: Byte): String = df.format(BigInt(s)) }

  implicit def shortSerializer(implicit df: IntegerFormat): StringSerializer[Short] =
    new StringSerializer[Short] { def serialize(s: Short): String = df.format(BigInt(s)) }

  implicit def longSerializer(implicit df: IntegerFormat): StringSerializer[Long] =
    new StringSerializer[Long] { def serialize(s: Long): String = df.format(BigInt(s)) }

  implicit def intSerializer(implicit df: IntegerFormat): StringSerializer[Int] =
    new StringSerializer[Int] { def serialize(s: Int): String = df.format(BigInt(s)) }

  implicit val stringSerializer: StringSerializer[String] =
    new StringSerializer[String] { def serialize(s: String): String = s }

  implicit def doubleSerializer(implicit df: DecimalFormat): StringSerializer[Double] =
    new StringSerializer[Double] { def serialize(s: Double): String = df.format(BigDecimal(s)) }
  
  implicit def floatSerializer(implicit df: DecimalFormat): StringSerializer[Float] =
    new StringSerializer[Float] { def serialize(f: Float): String = df.format(BigDecimal(f.toDouble)) }
  
  implicit def bigDecimalSerializer(implicit df: DecimalFormat): StringSerializer[BigDecimal] =
    new StringSerializer[BigDecimal] { def serialize(s: BigDecimal): String = df.format(s) }
  
  implicit def bigIntSerializer(implicit df: IntegerFormat): StringSerializer[BigInt] =
    new StringSerializer[BigInt] { def serialize(s: BigInt): String = df.format(s) }
}

/** A generic string serializer */
@implicitNotFound("It is not possible to serialize a value of type ${T} to a String without a"+
    " valid StringSerializer instance in scope.")
trait StringSerializer[-T] { stringSerializer =>
  def serialize(string: T): String
  def contramap[S](fn: S => T): StringSerializer[S] = new StringSerializer[S] {
    def serialize(string: S): String = stringSerializer.serialize(fn(string))
  }
}

object String {
  
  def apply[T: StringSerializer](t: T): String =
    ?[StringSerializer[T]].serialize(t)

  // Proxied from java.lang.String
  def format(str: String, any: AnyRef*) = java.lang.String.format(str, any: _*)
  
  def format(locale: java.util.Locale, str: String, any: AnyRef*) =
    java.lang.String.format(locale, str, any: _*)

  def copyValueOf(arr: Array[Char]): String = java.lang.String.copyValueOf(arr)

  def vauleOf(x: Array[Char]): String = java.lang.String.valueOf(x)
  def vauleOf(x: Boolean): String = java.lang.String.valueOf(x)
  def vauleOf(x: Double): String = java.lang.String.valueOf(x)
  def vauleOf(x: Int): String = java.lang.String.valueOf(x)
  def vauleOf(x: Any): String = java.lang.String.valueOf(x)
  def vauleOf(x: Array[Char], a: Int, b: Int): String = java.lang.String.valueOf(x, a, b)
  def vauleOf(x: Char): String = java.lang.String.valueOf(x)
  def vauleOf(x: Float): String = java.lang.String.valueOf(x)
  def vauleOf(x: Long): String = java.lang.String.valueOf(x)

  val CASE_INSENSITIVE_ORDER = java.lang.String.CASE_INSENSITIVE_ORDER
}
