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

import scala.collection.generic.CanBuildFrom

import language.higherKinds

object `package` {
  implicit def bytesParser(implicit enc: Encoding): StringParser[Bytes] { type Throws = Nothing } = new StringParser[Bytes] {
    type Throws = Nothing // We would like to throw an EncodingException if we try to decode an invalid byte
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Bytes, Nothing] = mode.wrap {
      Bytes(s.getBytes("UTF-8"))
    }
  }
}

trait FromBytes[T] { def build(bytes: Array[Byte]): T }

object FromBytes {
  implicit def stringFromBytes(implicit enc: Encoding): FromBytes[String] = new FromBytes[String] {
    def build(bytes: Array[Byte]): String = new String(bytes, enc.name)
  }

  implicit def bytesFromBytes = new FromBytes[Array[Byte]] {
    def build(bytes: Array[Byte]): Array[Byte] = bytes
  }
}

trait `decode.apply` extends MethodConstraint
case class DecodeException(position: Option[Int]) extends Exception

trait CodecType
trait Hex extends CodecType
trait Base64 extends CodecType
trait Base64Url extends CodecType
trait Base32 extends CodecType
trait Binary extends CodecType

object decode {
  def apply[C <: CodecType: ByteCodec](s: String)(implicit mode: Mode[`decode.apply`]): mode.Wrap[Bytes, DecodeException] =
    mode wrap {
      try {
        implicitly[ByteCodec[C]].decode(s) match {
          case Left(pos) => mode.exception(DecodeException(Some(pos)))
          case Right(res) => Bytes(res)
        }
      } catch { case e: Exception => mode.exception(DecodeException(None)) }
    }
}

object ByteCodec {
  implicit val base64Url: ByteCodec[Base64Url] = new Base64Codec('-', '_', '=', false, false)
  implicit val base64: ByteCodec[Base64] = new Base64Codec('+', '/', '=', false, false)

  implicit val hex: ByteCodec[Hex] = new ByteCodec[Hex] {
    def encode(array: Array[Byte]): String =
      alloc(array flatMap { n =>
        Array((n & 255) >> 4 & 15, n & 15)
      } map { _ + 48 } map { i =>
        (if(i > 57) i + 39 else i).toChar
      })

    def decode(s: String): Either[Int, Array[Byte]] =
      Right((if(s.length%2 == 0) s else "0"+s).to[Array].grouped(2).to[Array] map { case Array(l, r) =>
        (((l - 48)%39 << 4) + (r - 48)%39).toByte
      })
  }

  implicit val binary: ByteCodec[Binary] = new ByteCodec[Binary] {
    def encode(array: Array[Byte]): String =
      alloc(Array.range(0, array.length*8) map { i =>
        if((array(i/8) & (1 << (7 - i%8))) > 0) 49.toByte else 48.toByte
      })

    def decode(s: String): Either[Int, Array[Byte]] = {
      null
    }
  }
}

trait ByteCodec[Codec <: CodecType] {
  def encode(bytes: Array[Byte]): String
  def decode(string: String): Either[Int, Array[Byte]]
}

object Bytes {
  implicit def arrayBytes(bytes: Array[Byte]): Bytes = Bytes(bytes)
}

case class Bytes(bytes: Array[Byte]) {
  def encode[Codec <: CodecType: ByteCodec]: String =
    implicitly[ByteCodec[Codec]].encode(bytes)
  
  override def toString = encode[Hex]

  def ++(that: Bytes): Bytes = Bytes(bytes ++ that.bytes)

  override def equals(that: Any) = that match {
    case Bytes(bs) =>
      bs.length == bytes.length && (bs zip bytes forall { case (a, b) => a == b })
    case _ =>
      false
  }

  override def hashCode = bytes.foldLeft(bytes.length)(_*131 + _)

  /** Sets all values in the underlying byte array to zeroes. This is useful if the `Bytes`
    * instance was storing sensitive data, such as a private key. */
  def zero() = bytes.indices foreach { bytes(_) = 0 }

  def as[T: FromBytes](implicit mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, DecodeException] = mode.wrap {
    try ?[FromBytes[T]].build(bytes) catch {
      case e: Exception => mode.exception[T, DecodeException](DecodeException(None))
    }
  }

  def apply(index: Int): Byte = bytes(index)

  def slice(start: Int, end: Int) = Bytes(bytes.slice(start, end))

  def length: Int = bytes.length

  def to[Coll[_]](implicit cbf: CanBuildFrom[Nothing, Byte, Coll[Byte]]): Coll[Byte] =
    bytes.to[Coll]
}

