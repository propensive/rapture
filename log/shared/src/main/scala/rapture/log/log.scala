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

package rapture.log

import rapture.base._
import rapture.core._
import rapture.io._

import scala.annotation._

import language.experimental.macros

object `package` {
  val log = Log

  implicit class LogStringContext(sc: StringContext) {
    def log(xs: rapture.log.parts.Part*): Spec = {
      new Spec {
        def render(level: Int, lineNo: Int, source: String) = {
          val sb = new StringBuilder
          for (i <- 0 until (sc.parts.length - 1)) {
            sb.append(sc.parts(i))
            sb.append(xs(i).fit(level, lineNo, source))
          }
          sb.append(sc.parts.last)
          sb.toString
        }
      }
    }
  }
}

trait Outputter {
  def log[L <: LogLevel](prefix: String, msg: Array[String])
}

object LogAction {
  implicit def defaultLogAction(implicit act: NamedLogAction): LogAction =
    new LogAction { def level = -1 }
}

@implicitNotFound(
    "You have not specified a valid logging level. Please import one of logLevels" +
    ".{trace._, debug._, info._, warn._, error._, fatal._}.")
trait LogAction {
  def level: Int
}

class NamedLogAction(val level: Int, val name: String) extends LogAction

object Loggable {
  implicit val stringLoggable: Loggable[String] = new Loggable[String] {
    def toArray(msg: String) = Array(msg)
  }

  implicit val exceptionLoggable: Loggable[Throwable] =
    new Loggable[Throwable] {
      def toArray(msg: Throwable) =
        Array(msg.toString) ++ msg.getStackTrace.map("        at " + _)
    }
}
trait Loggable[-Msg] { def toArray(msg: Msg): Array[String] }

object SourceContext {
  implicit def sourceContext: SourceContext = macro LogMacros.sourceContextMacro
}
case class SourceContext(lineNo: Int, sourceFile: String)

object LogMacros {
  def logMacro[Msg : c.WeakTypeTag](level: Int)(c: BlackboxContext)(
      msg: c.Expr[Msg])(log: c.Expr[Log],
                        loggable: c.Expr[Loggable[Msg]],
                        srcCtx: c.Expr[SourceContext]): c.Expr[Unit] = {
    import c.universe._
    val lev = c.Expr[Int](Literal(Constant(level)))
    reify {
      if (log.splice.action.level <= lev.splice) {
        log.splice.out.log(log.splice.spec.render(lev.splice,
                                                  srcCtx.splice.lineNo,
                                                  srcCtx.splice.sourceFile),
                           loggable.splice.toArray(msg.splice))
      }
    }
  }

  def sourceContextMacro(c: BlackboxContext): c.Expr[SourceContext] = {
    import c.universe._
    val lineNo = c.Expr[Int](Literal(Constant(c.enclosingPosition.line)))
    val sourceFile =
      c.Expr[String](Literal(Constant(c.enclosingPosition.source.toString)))
    reify { SourceContext(lineNo.splice, sourceFile.splice) }
  }

  def traceMacro[Msg : c.WeakTypeTag](c: BlackboxContext)(msg: c.Expr[Msg])(
      log: c.Expr[Log],
      loggable: c.Expr[Loggable[Msg]],
      sourceContext: c.Expr[SourceContext]): c.Expr[Unit] =
    logMacro(0)(c)(msg)(log, loggable, sourceContext)

  def debugMacro[Msg : c.WeakTypeTag](c: BlackboxContext)(msg: c.Expr[Msg])(
      log: c.Expr[Log],
      loggable: c.Expr[Loggable[Msg]],
      sourceContext: c.Expr[SourceContext]): c.Expr[Unit] =
    logMacro(1)(c)(msg)(log, loggable, sourceContext)

  def infoMacro[Msg : c.WeakTypeTag](c: BlackboxContext)(msg: c.Expr[Msg])(
      log: c.Expr[Log],
      loggable: c.Expr[Loggable[Msg]],
      sourceContext: c.Expr[SourceContext]): c.Expr[Unit] =
    logMacro(2)(c)(msg)(log, loggable, sourceContext)

  def warnMacro[Msg : c.WeakTypeTag](c: BlackboxContext)(msg: c.Expr[Msg])(
      log: c.Expr[Log],
      loggable: c.Expr[Loggable[Msg]],
      sourceContext: c.Expr[SourceContext]): c.Expr[Unit] =
    logMacro(3)(c)(msg)(log, loggable, sourceContext)

  def errorMacro[Msg : c.WeakTypeTag](c: BlackboxContext)(msg: c.Expr[Msg])(
      log: c.Expr[Log],
      loggable: c.Expr[Loggable[Msg]],
      sourceContext: c.Expr[SourceContext]): c.Expr[Unit] =
    logMacro(4)(c)(msg)(log, loggable, sourceContext)

  def fatalMacro[Msg : c.WeakTypeTag](c: BlackboxContext)(msg: c.Expr[Msg])(
      log: c.Expr[Log],
      loggable: c.Expr[Loggable[Msg]],
      sourceContext: c.Expr[SourceContext]): c.Expr[Unit] =
    logMacro(5)(c)(msg)(log, loggable, sourceContext)
}

abstract class Spec {
  def render(level: Int, lineNo: Int, source: String): String
}

/*object Main extends App {
  import logLevels.debug._
  import encodings.system
  implicit val output = Logger(Stdout)
  implicit val spec = log"  "
  Log.logImplicit
  log.info("Here is an information message.")
  sys.exit(1)
}*/

object Log {
  implicit def logImplicit(
      implicit spec: Spec, out: Outputter, action: LogAction) =
    Log(spec, out, action)

  def trace[Msg](msg: Msg)(
      implicit log: Log,
      loggable: Loggable[Msg],
      sourceContext: SourceContext): Unit = macro LogMacros.traceMacro[Msg]

  def debug[Msg](msg: Msg)(
      implicit log: Log,
      loggable: Loggable[Msg],
      sourceContext: SourceContext): Unit = macro LogMacros.debugMacro[Msg]

  def info[Msg](msg: Msg)(
      implicit log: Log,
      loggable: Loggable[Msg],
      sourceContext: SourceContext): Unit = macro LogMacros.infoMacro[Msg]

  def warn[Msg](msg: Msg)(
      implicit log: Log,
      loggable: Loggable[Msg],
      sourceContext: SourceContext): Unit = macro LogMacros.warnMacro[Msg]

  def error[Msg](msg: Msg)(
      implicit log: Log,
      loggable: Loggable[Msg],
      sourceContext: SourceContext): Unit = macro LogMacros.errorMacro[Msg]

  def fatal[Msg](msg: Msg)(
      implicit log: Log,
      loggable: Loggable[Msg],
      sourceContext: SourceContext): Unit = macro LogMacros.fatalMacro[Msg]
}
case class Log(spec: Spec, out: Outputter, action: LogAction)

case class Logger[Res](res: Res)(implicit appender: Appender[Res, String])
    extends Outputter {

  final val maxDelay = 25
  final val queueThreshold = 10

  private val queue = new scala.collection.mutable.Queue[String]
  private var blankPrefix: String = null

  private val thread: Thread = Thread.fork("rapture-log") {
    var continue = true
    sys.addShutdownHook { continue = false }

    while (continue) {
      try {
        queue synchronized {
          queue.wait(maxDelay)
          if (queue.size > 0)
            res handleAppend { out: Output[String] =>
              while (queue.size > 0) out.write(queue.dequeue)
            }
        }
      } catch {
        case e: InterruptedException =>
          continue = false
      }
    }
  }

  def log[L <: LogLevel](prefix: String, msg: Array[String]) =
    queue synchronized {
      if (blankPrefix == null) blankPrefix = (" " * (prefix.length + 1))
      queue.enqueue(prefix + " " + msg(0))
      msg.tail foreach { m =>
        queue.enqueue(blankPrefix + m)
      }
      if (queue.size >= queueThreshold) queue.notify()
    }
}

object stdoutLogging {
  import parts._
  implicit def implicitSpec(implicit severity: Severity,
                            date: Date,
                            time: Time,
                            thread: Thread): Spec =
    log"""$date $time [$severity] ${sourceFile(width = 12, Right)}:${lineNo(4)} ${thread(
        10)}"""

  implicit val output = Logger(Stdout)

  def apply()(implicit severity: Severity,
              date: Date,
              time: Time,
              thread: Thread): Spec with Outputter = new Spec with Outputter {

    def render(level: Int, lineNo: Int, source: String): String =
      implicitSpec.render(level, lineNo, source)

    def log[L <: LogLevel](prefix: String, msg: Array[String]) =
      output.log(prefix, msg)
  }
}
