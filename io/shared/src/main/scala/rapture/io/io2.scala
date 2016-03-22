package rapture.io2

import scala.annotation.unchecked._
import rapture.core._

trait Input[+U] {
  def termination: U
  def read(): U
}

trait Output[-U] {
  def write(u: U): Unit
}

trait StreamInput[+U] {
  def read(array: Array[U @uncheckedVariance]): Int
}

trait StreamOutput[-U] {
  def write(array: Array[U @uncheckedVariance], start: Int, end: Int)
}

trait `Inputtable#input` extends MethodConstraint
trait `Outputtable#output` extends MethodConstraint

trait Inputtable[+U, -Res] {
  def input(res: Res): Input[U]
}

object Inputtable {
  class Capability[U, Res] {
    type Exc <: Exception
    def input(res: Res)(implicit mode: Mode[`Inputtable#input`], inputtable: Inputtable[U, Res]): mode.Wrap[Input[U], Exc] =
      mode wrap inputtable.input(res)
  }
}

trait Outputtable[-U, -Res] {
  def output(res: Res): Output[U]
}

object Outputtable {
  class Capability[U, Res] {
    type Exc <: Exception
    def output(res: Res)(implicit mode: Mode[`Outputtable#output`], outputtable: Outputtable[U, Res]): mode.Wrap[Output[U], Exc] =
      mode wrap outputtable.output(res)
  }
}

package ioBackends {

  object javaIo {
    class JavaReaderInput(val reader: java.io.Reader) extends Input[Char] {
      def termination = -1.toChar
      def read(): Char = reader.read().toChar
    }

    class JavaWriterInput(val writer: java.io.Writer) extends Output[Char] {
      def write(u: Char) = writer.write(u.toInt)
    }
  }
}
