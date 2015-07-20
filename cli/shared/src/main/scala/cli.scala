/******************************************************************************************************************\
* Rapture CLI, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                      *
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

package rapture.cli

import rapture.io._
import rapture.codec._
import rapture.fs._
import rapture.core._
import rapture.uri._
import rapture.log._

import annotation.tailrec

import encodings.system._
import logLevels.trace._

import scala.collection.immutable.ListMap

import language.higherKinds
import language.experimental.macros

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

object DebugModeConfig {
  implicit val defaultDebugMode: DebugModeConfig = DebugModeConfig(false)
}

object debugMode {
  def apply(): DebugModeConfig = implicitDebugMode
  implicit val implicitDebugMode: DebugModeConfig = DebugModeConfig(true)
}

case class DebugModeConfig(on: Boolean)

object Pwd {
  implicit val defaultPwd: Pwd = Pwd(File.home)
}

case class Pwd(file: FileUrl) extends AnyVal

object ShParam {
  implicit def stringableToShParam[T: StringSerializer](t: T): ShParam =
    ShParam(Vector(?[StringSerializer[T]].serialize(t)))

  implicit def genSeqSerializer[T: StringSerializer, Coll[E] <: TraversableOnce[E]](ts: Coll[T]): ShParam =
    ShParam(ts.map(?[StringSerializer[T]].serialize(_)).to[Vector])
}

case class ShParam(elems: Vector[String])

object `package` {
  implicit class ProcessStringContext(sc: StringContext) {
    def sh(content: ShParam*): Process = {
      var out: Vector[String] = sc.parts.head.split(" ").to[Vector]
      for((v, f) <- content zip sc.parts.tail) {
        out ++= v.elems
        out ++= f.split(" ").to[Vector]
      }
      Process(out: _*)
    }
  }
  
  implicit val logger = Logger(uri"file:///tmp/rapture-cli/access.log")
  import rapture.log.parts._
  implicit def implicitSpec(implicit severity: Severity, date: Date, time: Time, thread: Thread): Spec =
    log"""$date $time $severity ${sourceFile(width = 12, Right)}:${lineNo(4)} ${thread(14)}"""
}

sealed class CliException(msg: String) extends Exception(msg)

case class ParamGetException(name: String) extends CliException(s"Missing parameter $name")

abstract class BackgroundCliApp(implicit debugMode: DebugModeConfig) extends CliApp with Completions[Zsh with Bash] {

  val shellCompleter = new Bash with Zsh {}

  private var lastExitStatus = 0

  override def doExit(code: Int) = lastExitStatus = code

  def shutdown(): Unit = ()

  override def main(args: Array[String]) = {
    val appName = args(0)
    val fifo = File.parse(s"file:///tmp/rapture-cli/${appName}.sock")
    var continue = true
    var invocation = 0
    while(continue) {
      val msg = fifo.slurp[Char].trim
      msg.split(",").to[List].map(_.urlDecode) match {
        case "shutdown" :: Nil =>
          log.info("Received shutdown command")
          shutdown()
          fifo.delete()
          sys.exit(0)
        case "sigint" :: file :: Nil =>
          log.info("Received SIGINT for file "+file)
        case "winch" :: file :: lines :: cols :: Nil =>
          log.info(s"Received SIGWINCH for file $file $lines x $cols")
        case "exec" :: file :: pwd :: rest =>
          log.info(s"Using pwd = $pwd")
          val ps = new java.io.PrintStream(new java.io.FileOutputStream(new java.io.File(file)))
          invocation += 1
          Future {
            try {
              System.setOut(ps)
              try super.run(File.parse(s"file://$pwd"), rest.to[Array]) catch {
                case e: Throwable => if(debugMode.on) e.printStackTrace()
              }
            } catch {
              case e: Throwable =>
                if(debugMode.on) e.printStackTrace()
            } finally ps.close()
            val ps2 = new java.io.PrintStream(new java.io.FileOutputStream(new java.io.File(s"$file.exit")))
            try {
              ps2.println(lastExitStatus.toString)
              ps2.flush()
              ps2.close()
              System.setOut(sysOut)
            } finally ps2.close()
          }
        case _ =>
      }
    }
  }
}

case class ReturnEarly() extends Exception()

abstract class CliApp(implicit debugMode: DebugModeConfig) {
  private val NoOutput = new java.io.PrintStream(new java.io.OutputStream() {
    def write(x: Int) = ()
  })

  def exit(code: Int): Unit = throw Exit(code)
  def exec(block: => Unit): Exec = Exec((out: java.io.PrintStream) => block)
  def exec(block: java.io.PrintStream => Unit): Exec = Exec(block)
  val sysOut = System.out
  
  def doExit(code: Int): Unit = sys.exit(code)

  def main(args: Array[String]): Unit = run(File.parse(s"file://${System.getenv("PWD")}"), args)

  def run(pwd: FileUrl, args: Array[String]): Unit = {
    
    val exitStatus: Exit = try {
      Console.withOut(NoOutput) {
        try {
          val cmdLine: CmdLine = makeCmdLine(pwd, args.to[Vector])
          val execution = handle(cmdLine)
          
          if(cmdLine.completer.isEmpty) {
            execution.exec(System.out)
            Exit(0)
          } else Exit(0)
        } catch {
          case ReturnEarly() =>
            Exit(0)
          case err: Throwable =>
            Console.withOut(sysOut) {
              println("Unexpected error")
              if(debugMode.on) err.printStackTrace()
            }
            throw Exit(1)
        }
      }
    } catch { case err@Exit(_) => err }

    doExit(exitStatus.code)
  }

  def makeCmdLine(pwd: FileUrl, args: Vector[String]) =
    CmdLine(pwd, args map { s => Arg(s, None, false) }, None)
  
  def handle(cmdLine: CmdLine): Exec
}

trait Shell {
  def makeCmdLine(pwd: FileUrl, cmdLine: Vector[String]): CmdLine =
    CmdLine(pwd, cmdLine.map(Arg(_, None, false)), None)
}

trait Zsh extends Shell {
  override def makeCmdLine(pwd: FileUrl, cmdLine: Vector[String]): CmdLine = cmdLine match {
    case "---rapture-zsh" +: prefix +: cursor +: cols +: "--" +: rest =>
      val colWidth = cols.substring(10).toInt
      val cur = cursor.substring(9).toInt
      val words = if(cur >= rest.length) rest.tail :+ "" else rest.tail
      val completer = Completer(prefix.substring(9).urlDecode, zshCompleter(_, colWidth))
      CmdLine(pwd, words.zipWithIndex map { case (s, idx) =>
        Arg(s, Some(completer), cur - 2 == idx)
      }, Some(completer))
    case _ =>
      super.makeCmdLine(pwd, cmdLine)
  }

  def zshCompleter(suggestions: Suggestions, colWidth: Int): Nothing = {
    suggestions.groups.map { g =>
      val cmds = Compadd(g.title, g.suggestions.keys.to[Vector], true,
          v => g.suggestions(v), colWidth, g.hidden)
      cmds foreach System.out.println
    }
    throw ReturnEarly()
  }
}

trait Bash extends Shell {
  override def makeCmdLine(pwd: FileUrl, cmdLine: Vector[String]): CmdLine = cmdLine match {
    case "---rapture-bash" +: prefix +: cursor +: cols +: "--" +: rest =>
      val colWidth = cols.substring(10).toInt
      val words = if(cursor.toInt - 1 >= rest.length) rest.tail :+ "" else rest.tail
      val completer = new Completer(prefix.urlDecode, bashCompleter)
      CmdLine(pwd, words.zipWithIndex map { case (s, idx) =>
        Arg(s, Some(completer), cursor.toInt - 2 == idx)
      }, Some(completer))
    case _ =>
      super.makeCmdLine(pwd, cmdLine)
  }

  def bashCompleter(suggestions: Suggestions): Nothing = {
    System.out.println("Using bash")
    throw ReturnEarly()
  }
}

case class Exit(code: Int) extends Exception
case class Exec(exec: java.io.PrintStream => Unit)

