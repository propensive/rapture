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
package rapture.cli

import rapture.io._
import rapture.core._
import rapture.codec._
import java.io.{Reader => _, _}

import encodings.system._
import language.higherKinds

trait `Process#exec` extends MethodConstraint

case class Process(params: Vector[String]) {
  def exec[T: ProcessInterpreter](implicit mode: Mode[`Process#exec`], env: Environment):
      mode.Wrap[T, CliException] = mode.wrap {
    val javaProcess = Runtime.getRuntime().exec(params.to[Array],
        env().map { case (k, v) => s"$k=$v" }.to[Array],
	new File(env.workDir.getOrElse(System.getenv("HOME"))))
    val stream = new ByteInput(new BufferedInputStream(javaProcess.getInputStream))
    val stderr = new ByteInput(new BufferedInputStream(javaProcess.getErrorStream))
    ?[ProcessInterpreter[T]].interpret(stream, stderr, () => javaProcess.waitFor())
  }

  override def toString = {
    val escaped = params.map(_.flatMap {
      case '\'' => "\\'"
      case '"' => "\\\""
      case '\\' => "\\\\"
      case ' ' => "\\ "
      case chr => chr.toString
    })
    
    s"""sh"${escaped.mkString(" ")}""""
  }
}

object Environment {
  implicit val defaultEnvironment = new Environment {
    def apply(): Map[String, String] = {
      import scala.collection.JavaConversions._
      System.getenv().toMap
    }

    def workDir: Option[String] = Option(System.getProperty("user.dir"))
  }
}
trait Environment {
  def apply(): Map[String, String]
  def workDir: Option[String]
}

package environments {
  object empty {
    def apply(): Environment = implicitEnvironment
    implicit val implicitEnvironment: Environment =
      new Environment {
        def apply(): Map[String, String] = Map()
	def workDir = None
      }
  }

  object enclosing {
    def apply(): Environment = implicitEnvironment
    implicit val implicitEnvironment: Environment = Environment.defaultEnvironment
  }
}

object ProcessInterpreter {
  implicit val stringProcessInterpreter: ProcessInterpreter[String] =
    new ProcessInterpreter[String] {
      def interpret(input: Input[Byte], stderr: Input[Byte], exitStatus: () => Int): String = {
        val out = input.slurp[Char]
        val err = stderr.slurp[Char]
        exitStatus() match {
          case n => if(out == "" || out.last != '\n') out else out.init
          //case n => throw ShellProcessException(n, out.trim)
        }
      }
    }
 
  implicit def genSeqInterpreter[Coll[_], T](implicit cbf:
      collection.generic.CanBuildFrom[Nothing, T, Coll[T]], stringParser: StringParser[T]):
      ProcessInterpreter[Coll[T]] = new ProcessInterpreter[Coll[T]] {
    def interpret(input: Input[Byte], stderr: Input[Byte], exitStatus: () => Int):
        Coll[T] = {
      val out = input.slurp[Char]
      exitStatus() match {
        case 0 =>
          val builder = cbf()
          // FIXME: Reimplement this using a streaming method
          out.split("\n").foreach { s => builder += stringParser.parse(s, modes.throwExceptions()) }
          builder.result()
        case n =>
          throw ShellProcessException(n, out.trim)
      }
    }
  }

  implicit val bytesProcessInterpreter: ProcessInterpreter[Bytes] =
    new ProcessInterpreter[Bytes] {
      def interpret(input: Input[Byte], stderr: Input[Byte], exitStatus: () => Int): Bytes = {
        val out = input.slurp[Byte]
        exitStatus() match {
          case 0 => out
          case n => throw ShellProcessException(n, "Binary data")
        }
      }
    }
  
  implicit val byteInputProcessInterpreter: ProcessInterpreter[Input[Byte]] =
    new ProcessInterpreter[Input[Byte]] {
      def interpret(input: Input[Byte], stderr: Input[Byte], exitStatus: () => Int): Input[Byte] = input
    }
  
  implicit def inputProcessInterpreter[T](implicit rdr: Reader[Input[Byte], T]):
      ProcessInterpreter[Input[T]] =
    new ProcessInterpreter[Input[T]] {
      def interpret(input: Input[Byte], stderr: Input[Byte], exitStatus: () => Int): Input[T] =
        input.input[T]
    }
  
  implicit val intProcessInterpreter: ProcessInterpreter[Int] =
    new ProcessInterpreter[Int] {
      def interpret(input: Input[Byte], stderr: Input[Byte], exitStatus: () => Int): Int = exitStatus()
    }
}

trait ProcessInterpreter[+T] {
  def interpret(input: Input[Byte], stderr: Input[Byte], exitStatus: () => Int): T
}

case class ShellProcessException(exitStatus: Int, output: String) extends
    Exception("Shell process returned non-zero exit status: "+exitStatus)
