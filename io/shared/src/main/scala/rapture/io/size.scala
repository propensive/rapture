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

import scala.reflect._

object Sizable {
  class Capability[Res](res: Res) {
    /** Returns the size in bytes of this resource */
    def size[Data](implicit mode: Mode[`Sizable#size`], sizable: Sizable[Res, Data]): mode.Wrap[Long, Exception] =
      mode wrap sizable.size(res)
  }

  implicit def charSizable[Res, Data: ClassTag](implicit reader: Reader[Res, Data]): Sizable[Res, Data] = new Sizable[Res, Data] {
    private def accumulator() = new Accumulator[Data, Long] with Output[Data] {
      private var count = 0
      def buffer: Long = count
      def write(b: Data) = count += 1
      def flush(): Unit = ()
      def close(): Unit = ()
    }

    def size(res: Res): Long = {
      val acc = accumulator()
      res.handleInput[Data, Int](_ pumpTo acc)
      acc.buffer
    }
  }
}

trait Sizable[Res, Data] {
  type ExceptionType <: Exception
  /** Returns the number of units of the specified resource */
  def size(res: Res): Long
}


