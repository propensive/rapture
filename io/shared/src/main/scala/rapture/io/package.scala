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
package rapture.io

import rapture.core._
import rapture.uri._
import java.io._

trait `Appendable#appendOutput` extends MethodConstraint
trait `Readable#input` extends MethodConstraint
trait `Readable#redirectTo` extends MethodConstraint
trait `Readable#appendTo` extends MethodConstraint
trait `Writable#output` extends MethodConstraint
trait `Sizable#size` extends MethodConstraint
trait `Slurpable#slurp` extends MethodConstraint
trait `Copyable#copyTo` extends MethodConstraint
trait `Movable#moveTo` extends MethodConstraint
trait `Deletable#delete` extends MethodConstraint

object `package` {
  
  /** Views an `Input[Byte]` as a `java.io.InputStream` */
  implicit def inputStreamUnwrapper(is: Input[Byte]): InputStream =
    new InputStream { def read() = is.read().map(_.toInt).getOrElse(-1) }

  implicit def classpathStreamByteReader(implicit cl: ClassLoader): JavaInputStreamReader[ClasspathUrl] =
    ClasspathStream.classpathStreamByteReader

  def ensuring[Res, Strm: Closable](create: Strm)(body: Strm => Res):
      Res = Utils.ensuring[Res, Strm](create)(body)

  implicit def stringMethods(s: String): StringMethods = alloc(s)
  implicit def copyable[Res](res: Res): Copyable.Capability[Res] = alloc(res)
  implicit def appendable[Res](res: Res): Appendable.Capability[Res] = alloc(res)
  implicit def readable[Res](res: Res): Readable.Capability[Res] = alloc(res)
  implicit def deletable[Res](res: Res): Deletable.Capability[Res] = alloc(res)
  implicit def slurpable[Res](res: Res): Slurpable.Capability[Res] = alloc(res)
  implicit def writable[Res](res: Res): Writable.Capability[Res] = alloc(res)
  implicit def movable[Res](res: Res): Movable.Capability[Res] = alloc(res)
  implicit def sizable[Res](res: Res): Sizable.Capability[Res] = alloc(res)
  
}
