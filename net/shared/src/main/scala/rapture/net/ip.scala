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
package rapture.net

import rapture.core._
import rapture.codec._

object Ipv6 {
  def parse(s: String)(implicit mode: Mode[`Ipv6.parse`]): mode.Wrap[Ipv6, Exception] =
    mode.wrap {
      val groups: Array[String] = s.split("::").map(_.split(":")) match {
        case Array() => Array.fill(8)("")
        case Array(xs) => xs.padTo(8, "")
        case Array(Array(""), xs) => Array.fill(8 - xs.length)("") ++ xs
        case Array(xs, ys) => xs ++ Array.fill(8 - xs.length - ys.length)("") ++ ys
      }
      val gs = groups map (decode[Hex](_).bytes) map {
        case Array() => 0
        case Array(le) => le
        case Array(be, le) => (be << 8) + le
      }

      Ipv6(gs(0), gs(1), gs(2), gs(3), gs(4), gs(5), gs(6), gs(7))
    }
}

object Ipv4 {

  def parse(s: String) = {
    val vs = s.split("\\.").map(_.toInt)
    Ipv4(vs(0), vs(1), vs(2), vs(3))
  }
  
  def fromLong(lng: Long) =
    Ipv4((lng>>24 & 255L).toInt, (lng>>16 & 255L).toInt, (lng>>8 & 255L).toInt,
        (lng & 255L).toInt)
  
  lazy val privateSubnets = List(Ipv4(10, 0, 0, 0)/8, Ipv4(192, 168, 0, 0)/16,
      Ipv4(172, 16, 0, 0)/12, Ipv4(127, 0, 0, 0)/8)
}

case class Ipv6(s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int) {

  def groups = Vector(s1, s2, s3, s4, s5, s6, s7, s8)

  def expanded = groups map { s =>
    Bytes(Array(((s >> 8) & 0xff).toByte, (s & 0xff).toByte)).encode[Hex]
  } mkString ":"

  override def toString = expanded.replaceAll("^0+", "").replaceAll(":0+", ":").replaceAll("::+", "::")
}
case class Ipv4(b1: Int, b2: Int, b3: Int, b4: Int) {
  
  if(b1 > 255 || b2 > 255 || b3 > 255 || b4 > 255 || b1 < 0 || b2 < 0 || b3 < 0 || b4 < 0)
    throw new InstantiationException(
        "The components of the IP address must be in the range 0-255")

  def asLong = (b1.toLong<<24) + (b2<<16) + (b3<<8) + b4
  def /(i: Int): Subnet = new Subnet(this, i)
  def in(subnet: Subnet) = subnet contains this
  override def toString() = b1+"."+b2+"."+b3+"."+b4
  def isPrivate = Ipv4.privateSubnets.exists(in)

  override def equals(that: Any): Boolean = that match {
    case that: Ipv4 => b1 == that.b1 && b2 == that.b2 && b3 == that.b3 && b4 == that.b4
    case _ => false
  }

  override def hashCode = b1<<24 | b2<<16 | b3<<8 | b4
}

object Subnet {
  def parse(s: String) = {
    val x = s.split("\\/")
    new Subnet(Ipv4.parse(x(0)), x(1).toInt)
  }
}

class Subnet(baseIp: Ipv4, val bits: Int) extends Iterable[Ipv4] {
  if(bits < 0 || bits > 32)
    throw new InstantiationException("The subnet size must be in the range 0-32")

  def iterator: Iterator[Ipv4] = new Iterator[Ipv4] {
    private var current = baseIp.asLong - 1
    def hasNext = current < maximum.asLong
    def next = {
      current += 1
      Ipv4.fromLong(current)
    }
  }

  def maximum = Ipv4.fromLong((((baseIp.asLong>>(32 - bits)) + 1)<<(32 - bits)) - 1)
  val ip = Ipv4.fromLong((baseIp.asLong>>(32 - bits))<<(32 - bits))
  override def size = 1 << (32 - bits)
  override def toString() = ip.toString+"/"+bits
  def contains(ip2: Ipv4) = Ipv4.fromLong((ip2.asLong>>(32 - bits))<<(32 - bits)) == ip

  override def equals(that: Any) = that match {
    case that: Subnet => ip == that.ip && bits == that.bits
    case _ => false
  }

  override def hashCode = ip.hashCode | bits
}

object Localhost extends Ipv4(127, 0, 0, 1)
