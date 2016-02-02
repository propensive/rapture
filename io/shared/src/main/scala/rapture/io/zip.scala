/******************************************************************************************************************\
* Rapture IO, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                       *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in complance    *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.io

import rapture.core._
import rapture.codec._

import scala.reflect._

import java.io._

class ZipEntries[Z, Z2](headers: Iterator[Z]) {
  
  private val values: Iterator[Z] = headers
  private val fn: Z => Z2
  private val proc

  def filter(pred: Z => Boolean): ZipEntries[Z] = new ZipEntries[T] {
    
  }
  
  def map[T](fn: Z => ZipEntry => T): ZipEntries[T] = new ZipEntries[T] {

  }

  def process(): Map[ZipMetadata, T]
}

case class ZipMetadata() {
  
}

case class ZipHeader(signature: Int, crc32: Int, compressedSize: Int, uncompressedSize: Int) {

}

object Unzippable {
  class Capability[Res](res: Res) {
    def zipEntries()(implicit mode: Mode[`Unzippable#zipEntries`], sr: Reader[Res, Data],
        mf: ClassTag[Data]): mode.Wrap[ZipEntries, Exception] =
      mode.wrap {
        
      }
  }
}

