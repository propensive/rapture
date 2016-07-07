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

package rapture.text

import rapture.base._

object ansi {

  def apply[T](fn: Tty => T) = {
    val tty = new Tty()
    try fn(tty)
    catch {
      case e: Throwable =>
        print(normal(tty))
        throw e
    } finally print(normal(tty))
  }

  def esc(implicit tty: Tty) = 27.toChar
  def normal(implicit tty: Tty) = s"$esc[0m"
  def bold(implicit tty: Tty) = s"$esc[1m"
  def underline(implicit tty: Tty) = s"$esc[4m"
  def blink(implicit tty: Tty) = s"$esc[5m"
  def reverse(implicit tty: Tty) = s"$esc[7m"
  def nondisplayed(implicit tty: Tty) = s"$esc[8m"
  def black(implicit tty: Tty) = s"$esc[0;30m"
  def red(implicit tty: Tty) = s"$esc[0;31m"
  def green(implicit tty: Tty) = s"$esc[0;32m"
  def yellow(implicit tty: Tty) = s"$esc[0;33m"
  def blue(implicit tty: Tty) = s"$esc[0;34m"
  def magenta(implicit tty: Tty) = s"$esc[0;35m"
  def cyan(implicit tty: Tty) = s"$esc[0;36m"
  def white(implicit tty: Tty) = s"$esc[0;37m"
  def boldBlack(implicit tty: Tty) = s"$esc[1;30m"
  def boldRed(implicit tty: Tty) = s"$esc[1;31m"
  def boldGreen(implicit tty: Tty) = s"$esc[1;32m"
  def boldYellow(implicit tty: Tty) = s"$esc[1;33m"
  def boldBlue(implicit tty: Tty) = s"$esc[1;34m"
  def boldMagenta(implicit tty: Tty) = s"$esc[1;35m"
  def boldCyan(implicit tty: Tty) = s"$esc[1;36m"
  def boldWhite(implicit tty: Tty) = s"$esc[1;37m"

  def cursor(row: Int, col: Int)(implicit tty: Tty) = s"$esc[${row};${col}H"
  def up(n: Int = 1)(implicit tty: Tty) = s"$esc[${n}A"
  def down(n: Int = 1)(implicit tty: Tty) = s"$esc[${n}B"
  def right(n: Int = 1)(implicit tty: Tty) = s"$esc[${n}C"
  def left(n: Int = 1)(implicit tty: Tty) = s"$esc[${n}D"

  def readPassword(prompt: String)(implicit tty: Tty): String = {
    print(s"${prompt}$nondisplayed")
    val res = compatibility.readLine
    println(normal)
    res
  }

  class Tty private[text] ()
}
