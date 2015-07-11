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
import rapture.uri._
import rapture.mime._

import java.util.zip._
import java.io._

object Zip {

  def zip(data: Map[SimplePath, Input[Byte]], comment: String = null, level: Int = 9): Input[Byte] with TypedInput = {
    val baos: ByteArrayOutputStream = alloc()
    val zos: ZipOutputStream = alloc(baos)
    if(comment != null) zos.setComment(comment)
    zos.setLevel(level)

    for((k, in) <- data) {
      zos.putNextEntry(alloc(k.toString.substring(1)))
      in.pumpTo(alloc[ByteOutput](zos))
      zos.closeEntry()
    }
    zos.finish()
    new ByteArrayInput(baos.toByteArray) with TypedInput {
      def mimeType = MimeTypes.`application/zip`
    }
  }

  /** GZips an input stream. Note that the current implementation blocks until the input has
    * been read. Future implementations will return after the first read. */
  def gzip(in: Input[Byte]): Input[Byte] with TypedInput = {
    val baos: ByteArrayOutputStream = alloc()
    val gzos: GZIPOutputStream = alloc(baos)
    in.pumpTo(alloc[ByteOutput](gzos))
    gzos.finish()
    new ByteArrayInput(baos.toByteArray) with TypedInput {
      def mimeType = MimeTypes.`application/x-gzip`
    }
  }
}
