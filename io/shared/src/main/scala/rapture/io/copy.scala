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

package rapture.io
import rapture.core._

trait Copyable_1 {
  implicit def streamableCopyable[SrcType, DestType](implicit reader: Reader[SrcType, Byte],
                                                     writer: Writer[DestType, Byte]): Copyable[SrcType, DestType] =
    new Copyable[SrcType, DestType] {
      def copy(src: SrcType, dest: DestType): Copyable.Summary = {
        val bytes = reader.input(src) > writer.output(dest)
        Copyable.StreamSummary(bytes)
      }
    }
}

object Copyable extends Copyable_1 {

  trait Summary

  case class StreamSummary(bytes: Long) extends Summary {
    override def toString = s"streamed $bytes bytes"
  }

  class Capability[SrcType](from: SrcType) {
    def copyTo[DestType](to: DestType)(implicit mode: Mode[`Copyable#copyTo`],
                                       copyable: Copyable[SrcType, DestType]): mode.Wrap[Summary, Exception] =
      mode.wrap(?[Copyable[SrcType, DestType]].copy(from, to))
  }
}

trait Copyable[-SrcType, -DestType] {
  def copy(from: SrcType, to: DestType): Copyable.Summary
}
