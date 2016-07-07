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

package rapture.codec

import rapture.core._

@implicitNotFound(
    "Character encoding has not been provided. Please specify an implicit " +
      "Encoding value, e.g. import encodings.system._ or import encodings.`UTF-8`._.")
case class Encoding(name: String) { def index = name }

case class EncodingImplicit(name: String) {
  implicit val implicitEncoding: Encoding = Encoding(name)
  def apply() = implicitEncoding
}

/** Provides references to standard character encodings provided by Java. Encodings are
  * represented by instances of the Encoding case class, which is a simple wrapper over a String
  * of the encoding's name. Several standard encodings are provided and identified by the
  * encoding's canonical name for the avoidance of ambiguity. These instances will typically
  * require escaping with backticks in order to be referenced, however type safety will be
  * ensured. */
object encodings {

  implicit val `US-ASCII` = EncodingImplicit("US-ASCII")
  implicit val `windows-1250` = EncodingImplicit("windows-1250")
  implicit val `windows-1251` = EncodingImplicit("windows-1251")
  implicit val `windows-1252` = EncodingImplicit("windows-1252")
  implicit val `windows-1253` = EncodingImplicit("windows-1253")
  implicit val `windows-1254` = EncodingImplicit("windows-1254")
  implicit val `windows-1257` = EncodingImplicit("windows-1257")
  implicit val `ISO-8859-1` = EncodingImplicit("ISO-8859-1")
  implicit val `ISO-8859-2` = EncodingImplicit("ISO-8859-2")
  implicit val `ISO-8859-4` = EncodingImplicit("ISO-8859-4")
  implicit val `ISO-8859-5` = EncodingImplicit("ISO-8859-5")
  implicit val `ISO-8859-7` = EncodingImplicit("ISO-8859-7")
  implicit val `ISO-8859-9` = EncodingImplicit("ISO-8859-9")
  implicit val `ISO-8859-13` = EncodingImplicit("ISO-8859-13")
  implicit val `ISO-8859-15` = EncodingImplicit("ISO-8859-15")
  implicit val `KOI8-R` = EncodingImplicit("KOI8-R")
  implicit val `UTF-8` = EncodingImplicit("UTF-8")
  implicit val `UTF-16` = EncodingImplicit("UTF-16")
  implicit val `UTF-16BE` = EncodingImplicit("UTF-16BE")
  implicit val `UTF-16LE` = EncodingImplicit("UTF-16LE")

  /** The default file system encoding for this system */
  implicit lazy val system = EncodingImplicit(System.getProperty("file.encoding"))

  private val allEncodings: Map[String, Encoding] = Map(
      ("US-ASCII", `US-ASCII`()),
      ("windows-1250", `windows-1250`()),
      ("windows-1251", `windows-1251`()),
      ("windows-1252", `windows-1252`()),
      ("windows-1253", `windows-1253`()),
      ("windows-1254", `windows-1254`()),
      ("windows-1257", `windows-1257`()),
      ("ISO-8859-1", `ISO-8859-1`()),
      ("ISO-8859-2", `ISO-8859-2`()),
      ("ISO-8859-4", `ISO-8859-4`()),
      ("ISO-8859-5", `ISO-8859-5`()),
      ("ISO-8859-7", `ISO-8859-7`()),
      ("ISO-8859-9", `ISO-8859-9`()),
      ("ISO-8859-13", `ISO-8859-13`()),
      ("ISO-8859-15", `ISO-8859-15`()),
      ("KOI8-R", `KOI8-R`()),
      ("UTF-8", `UTF-8`()),
      ("UTF-16", `UTF-16`()),
      ("UTF-16BE", `UTF-16BE`()),
      ("UTF-16LE", `UTF-16LE`())
  )

  def lookup(enc: String): Encoding = allEncodings(enc)

}
