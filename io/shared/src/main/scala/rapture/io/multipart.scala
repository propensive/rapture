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
import rapture.mime._

import scala.collection.mutable.ListBuffer

case class Multipart(data: Array[Byte], headers: Map[String, String]) {
  def contentType: Option[MimeTypes.MimeType] =
    headers.get("Content-Type") map {
      case MimeTypes(t) => t
      case t => MimeTypes.MimeType(t)
    }

  lazy val contentDisposition: Option[(String, Map[String, String])] =
    headers.get("Content-Disposition") flatMap { v =>
      v.split("; *").to[List] match {
        case h :: t =>
          val m = t.map({ a =>
            val b = a.split("=")
            b(0) -> b(1)
          }).toMap
          Some(h -> m)
        case _ => None
      }
    }

  def filename: Option[String] = contentDisposition flatMap { case (_, m) => m.get("filename") }

  def name: Option[String] = contentDisposition flatMap { case (_, m) => m.get("name") }

  def disposition: Option[String] = contentDisposition.map(_._1)

}

class MultipartReader(boundary: String, in: java.io.InputStream, val sizeLimit: Int = 160)
    extends Input[Multipart] {
  
  private val bnd = ("--"+boundary).getBytes("ASCII")
  
  private var finished = false
  private var cued = next()

  def close(): Unit = in.close()
  def ready(): Boolean = cued.isDefined
  def read(): Option[Multipart] = {
    val r = cued
    cued = next()
    r
  }

  private def next(): Option[Multipart] = {
    
    var buf: Array[Byte] = null
    var count = -1
    val bufs = alloc[ListBuffer[Array[Byte]]]()

    var headers: Map[String, String] = Map()
    var dataStart = 0
    var boundmatch: Int = 0

    while(!finished) {
      var cur = in.read()
      
      if(buf != null && dataStart == 0 && (buf(count%65536) == 10 && cur == 13 ||
          buf(count%65536) == 13 && cur == 10)) {
        // do nothing
      } else if(buf != null && dataStart == 0 && (cur == 10 || cur == 13 &&
          buf(count%65536) == cur)) {
        dataStart = count + 1
        val next = in.read().toByte
        if(next != 10 && next != 13) {
          count += 1
          buf(count%65536) = next
        }
        headers = alloc[String](buf.slice(1, dataStart), "ISO-8859-1").split("\r").map({ h =>
          val i = h.indexOf(':')
          h.substring(0, i) -> h.substring(i + 2, h.length)
        }).toMap
      } else {
        count += 1
        
        if(cur == -1) {
          finished = true
          return None
        }
        
        if(count%65536 == 0) {
          if(count > sizeLimit) throw alloc[RuntimeException]("Upload size limit exceeded.")
          buf = alloc(65536)
          bufs += buf
        }
        
        buf(count%65536) = cur.toByte
        
        boundmatch = if(buf(count%65536) == bnd(boundmatch)) boundmatch + 1 else 0
        
        if(boundmatch == bnd.length) {
          val dataEnd = count - boundmatch - 1
          val size = dataEnd - dataStart
          
          if(size >= 0) {
            val res: Array[Byte] = alloc(size)
            var done = 0
            var i = 0
            var offset = dataStart
            while(done < size) {
              val chunk = List(65536 - offset, size - done).min
              System.arraycopy(bufs(i), offset, res, done, chunk)
              done += chunk
              offset = 0
              i += 1
            }
            
            return Some(Multipart(res, headers))
          } else {
            count = -1
            bufs.clear()
            buf = null
            boundmatch = 0
            dataStart = 0
          }
        }
      }
    }
    None
  }
}

