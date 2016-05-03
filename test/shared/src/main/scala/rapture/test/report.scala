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

package rapture.test

import rapture.text._

import java.io.PrintStream

trait Reporter {

  type Tag

  def title(text: String)(implicit tty: ansi.Tty)
  def startTask(text: String)(implicit tty: ansi.Tty): Tag
  def completeTask(tag: Tag, result: TestResult)(implicit tty: ansi.Tty): Unit
  def report(text: String, inset: Boolean = false)(
      implicit tty: ansi.Tty): Unit
  def summary(status: Int, text: String)(implicit tty: ansi.Tty): Unit
}

class BasicReporter(width: Int, out: PrintStream) extends Reporter {

  private var spaceAbove = false
  def mkSpace() = {
    if (!spaceAbove) out.println()
    spaceAbove = true
  }
  def output(text: String) {
    out.println(text)
    spaceAbove = false
  }

  class Tag(val text: String) {
    val t0 = System.currentTimeMillis
  }

  private var activeTags: Set[Tag] = Set()

  def startTask(text: String)(implicit tty: ansi.Tty): Tag = synchronized {
    val cutText = text.take(width - 20)
    val tag = new Tag(cutText)
    out.print(s"${ansi.normal}${cutText}")
    activeTags += tag

    tag
  }

  def completeTask(tag: Tag, result: TestResult)(
      implicit tty: ansi.Tty): Unit = synchronized {
    val t = System.currentTimeMillis - tag.t0
    out.print(" " * (width - tag.text.length - 20))
    val pad = " " * (5 - t.toString.length)
    val colorText = result match {
      case Success => s"${ansi.green}SUCCESS"
      case Failure(msg) => s"${ansi.yellow}FAILURE"
      case Error(e) => s"${ansi.red} ERROR "
    }

    output(
        s"${ansi.boldBlue}[ ${colorText}${ansi.boldBlue} ]${pad}${t} ms${ansi.normal}")
    activeTags -= tag
  }

  def report(text: String, inset: Boolean = false)(implicit tty: ansi.Tty) = {
    val indent = if (inset) s"    ${ansi.boldBlue}" else ansi.normal
    text split "\n" flatMap (_.grouped(width - 25)) map (indent + _) foreach output
  }

  def summary(status: Int, text: String)(implicit tty: ansi.Tty) = {
    val color = status match {
      case 1 => ansi.green
      case 0 => ansi.yellow
      case _ => ansi.red
    }
    text grouped (width - 4) foreach { ln =>
      output(s" ${color}* ${ansi.normal}${ln}")
    }
  }

  def title(text: String)(implicit tty: ansi.Tty) =
    text grouped (width - 4) foreach { ln =>
      output(s" ${ansi.green}* ${ansi.normal}${ln}")
    }
}
