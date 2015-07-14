/******************************************************************************************************************\
* Rapture Log, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                      *
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

package rapture.log

import rapture.core._

import language.experimental.macros

object parts {

  abstract class Part(width: Int, align: Alignment) { part =>
    def text(level: Int, lineNo: Int, source: String): String
    def fit(level: Int, lineNo: Int, source: String) = {
      val t = text(level, lineNo, source)
      if(t.length > width) t.substring(0, width)
      else if(align == Left) t.padTo(width, ' ')
      else " "*(width - t.length)+t
    }
   
    def apply(width: Int = width, align: Alignment = align): Part = new Part(width, align) {
      def text(level: Int, lineNo: Int, source: String) =
        part.text(level, lineNo, source)
    }
  }

  sealed trait Alignment
  case object Left extends Alignment
  case object Right extends Alignment

  object Severity {
    implicit def severity: Severity = new Severity()
  }

  class Severity() extends Part(5, Left) {
    def text(level: Int, lineNo: Int, source: String) = level match {
      case 0 => "TRACE"
      case 1 => "DEBUG"
      case 2 => "INFO"
      case 3 => "WARN"
      case 4 => "ERROR"
      case 5 => "FATAL"
    }
  }

  object Time {
    implicit def logTime = new Time(System.currentTimeMillis)
    val timeFormat = new java.text.SimpleDateFormat("HH:mm:ss.SSS")
  }

  class Time(time: Long) extends Part(12, Left) {
    def text(level: Int, lineNo: Int, source: String) = Time.timeFormat.format(time)
  }

  object Date {
    implicit def logDate = new Date(System.currentTimeMillis)
    val dateFormat = new java.text.SimpleDateFormat("dd-MMM-yyyy")
  }

  class Date(date: Long) extends Part(9, Left) {
    def text(level: Int, lineNo: Int, source: String) = Date.dateFormat.format(date)
  }

  object sourceFile extends Part(4, Left) {
    def text(level: Int, lineNo: Int, source: String) = source.toString
  }

  object lineNo extends Part(4, Left) {
    def text(level: Int, lineNo: Int, source: String) = lineNo.toString
  }

  object Thread {
    implicit def currentThread: Thread = new Thread(java.lang.Thread.currentThread.getName)
  }

  class Thread(name: String) extends Part(10, Left) {
    def text(level: Int, lineNo: Int, source: String) = name
  }
}

