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

import rapture.core._
import rapture.text._
import rapture.base._
import rapture.fs._

import scala.reflect._

import java.net.URLClassLoader

import scala.language.experimental.macros

object Util {
  private val entries: collection.mutable.HashMap[FsUrl, Seq[String]] =
    new collection.mutable.HashMap()

  def entriesFromZip(f: FsUrl): Seq[String] = {
    import java.util.zip._
    import scala.collection.JavaConversions._
    val zf = new ZipFile(f.javaFile)
    entries.getOrElseUpdate(f,
                            zf.entries
                              .to[List]
                              .filter(_.getName endsWith ".class")
                              .filter(!_.getName.contains("$"))
                              .map(_.getName.dropRight(6)))
  }

  def getAllZipfiles(): List[FsUrl] = {
    val urls: Array[java.net.URL] =
      java.lang.Thread.currentThread.getContextClassLoader
        .asInstanceOf[URLClassLoader]
        .getURLs
    urls
      .to[List]
      .map { u =>
        File.parse("file://" + u.getFile)
      }
      .filter(_.writable)
  }
}

/*object Main extends BackgroundCliApp {

  object Params extends Params(ClasspathParam, SuiteParam, TestParam, ShutdownParam)
  lazy val ShutdownParam = Param("shutdown", "K", "Shutdown the server")
  lazy val ClasspathParam = Param("classpath", "c", "Specify the classpath for the test")
  lazy val TestParam = Param("test", "t", "Specify a single test to run")
  lazy val SuiteParam = Param("suite", "s", "Specify the suite of tests to run")

  def handle(line: CmdLine) = line match {
    case Params(ps) =>
      val directory = line.pwd.children.filter(_.filename endsWith ".jar")
      
      val cp: List[FsUrl] = ps.get(ClasspathParam, Suggestions.from(directory)(_.filename, f =>
          (f.size/1024)+"KB")).map { f => List(line.pwd / f) }.getOrElse(Util.getAllZipfiles())

      val classes: Seq[String] = cp.flatMap(Util.entriesFromZip).map(_.replaceAll("\\/", "."))

      val allSuites = ClassLoader(cp).applyTo {
        classes.filter { n => try { classOf[TestSuite] isAssignableFrom Class.forName(n) } catch {
          case e: Throwable => false
        } }
      }
      
      val oneSuite = ps.get(SuiteParam, Suggestions.from(allSuites)(identity)).map(List(_))
      val suites = oneSuite.getOrElse(allSuites).flatMap { s =>
        Try(s -> Class.forName(s).newInstance().asInstanceOf[TestSuite]).toOption
      }
      
      exec { out =>
        val cols = 100
        implicit val reporter = new BasicReporter(cols, out)
        for((n, s) <- suites) {
          out.println(s"Running test suite ${Ansi.Blue}${n}${Ansi.Normal}")
          out.println(s"${Ansi.BoldBlack}${"-"*cols}")
          s.runAll()
          out.println()
        }
        if(suites.isEmpty) out.println("Usage: rtest [--classpath <classpath>] --suite <suite>")
      }
  }
}*/

sealed trait TestResult { def message: Option[String] }

case object Success extends TestResult { def message = None }
case class Failure(msg: String) extends TestResult { def message = Some(msg) }
case class Error(error: Throwable) extends TestResult {
  private val stack = error.getStackTrace
    .map(_.toString)
    .takeWhile(!_.startsWith("rapture.test"))

  private val stackString = stack.mkString("    ", "\n    ", "")

  def message =
    Some(s"Exception thrown during test: ${error.toString}\n${stackString}")
}

trait TestCase

trait TestSuite {

  abstract class Test extends TestCase { thisTest =>

    type Return

    def dependencies = Set[Test]()
    def action(): Return
    def check(run: () => Return): TestResult
    def name: String
    def runCheck(): TestResult =
      try check(action) catch { case e: Throwable => Error(e) }

    protected def compare[T](x: T, y: T): TestResult =
      if (x == y) Success
      else {
        Failure(s"Found $x, but expected $y")
      }

    def returns(chk: => Return)(implicit assigned: AssignedName) = new Test {
      type Return = thisTest.Return
      def name = assigned.name
      def action(): Return = thisTest.action()
      def check(run: () => Return): TestResult = {
        val result = run()
        val y = chk
        if (result == y) Success else Failure(s"Found $result but expected $y")
      }
    }

    def satisfies(chk: Return => Boolean)(implicit assigned: AssignedName) =
      new Test {
        type Return = thisTest.Return
        def name = assigned.name
        def action(): Return = thisTest.action()
        def check(run: () => Return): TestResult = {
          val result = run()
          if (chk(result)) Success
          else Failure(s"Result $result did not satisfy predicate.")
        }
      }

    def throws[E <: Throwable : ClassTag](cls: Class[E])(
        implicit assigned: AssignedName): Test = new Test {
      type Return = thisTest.Return
      def name = assigned.name
      def action(): Return = thisTest.action()
      def check(run: () => Return): TestResult =
        try {
          run()
          Failure("Expected exception not thrown.")
        } catch {
          case e: E => Success
          case e: Throwable =>
            Failure(
                s"Expected exception of type `${classTag[E]}', but found exception `${e}'.")
        }
    }

    def throws[E <: Throwable](exp: E)(implicit assigned: AssignedName): Test =
      new Test {
        type Return = thisTest.Return
        def name = assigned.name
        def action(): Return = thisTest.action()
        def check(run: () => Return): TestResult =
          try {
            run()
            Failure("Expected exception not thrown.")
          } catch {
            case e if e == exp => Success
            case e: Throwable =>
              Failure(s"Expected exception `$exp`, but found exception `$e`.")
          }
      }

    /*def apply(done: Set[Test] = Set())(implicit reporter: Reporter): TestSummary = {
      val (_, (success, count)) = dependencies.foldLeft((done, (0, 0))) {
        case ((d, (s0, n0)), t) =>
          val (s, n) = if(!d.contains(t)) t(d) else (0, 0)
          (d + t, (s0 + s, n0 + n))
      }
      
      val tag = reporter.startTask(s"${name}")
      val result = try check(action) catch { case e: Throwable => Error(e) }

      reporter.completeTask(tag, result)
      result.message foreach { s => reporter.report(s, inset = true) }

      TestSummary(success + (if(result == Success) 1 else 0), count + 1)
    }*/
  }

  def test[T](act: => T): Test { type Return = T } = new Test {

    type Return = T

    def name = "Unnamed test"
    def action(): Return = act
    def check(run: () => Return): TestResult = Success
  }
}

trait `run` extends MethodConstraint

case class TestSummary(successes: Int, failures: Int, errors: Int)

class BadTestResult(msg: String) extends Exception(msg)
case class TestFailure(name: String, error: String)
    extends BadTestResult(s"Test `$name` failed with error `$error`")

case class TestError(name: String, exception: Throwable)
    extends BadTestResult(s"Test `$name` failed with exception `$exception`")

object run {
  def apply[TS <: TestSuite](ts: TS)(implicit mode: Mode[`run`]): Any = macro run
    .runMacro[TS]

  def doTests(ts: List[TestSuite#Test],
              mode: Mode[`run`]): mode.Wrap[TestSummary, BadTestResult] =
    mode.wrap {
      implicit val reporter: Reporter = new BasicReporter(116, System.out)
      ansi { implicit tty =>
        val (successes, failures, errors) = ts.foldLeft((0, 0, 0)) {
          case ((s, f, e), t) =>
            val task = reporter.startTask(t.name)
            val result = t.runCheck()
            reporter.completeTask(task, result)
            result.message foreach { msg =>
              reporter.report(msg, inset = true)
            }
            result match {
              case Success =>
                (s + 1, f, e)
              case Failure(msg) =>
                mode.exception(TestFailure(t.name, msg))
                (s, f + 1, e)
              case Error(msg) =>
                mode.exception(TestError(t.name, msg))
                (s, f, e + 1)
            }
        }

        if (successes + failures + errors == 0)
          reporter.summary(0, "No tests found.")
        else if (failures + errors == 0)
          reporter.summary(1, "All tests passed.")
        else if (successes == 0) reporter.summary(-1, "All tests failed.")
        else
          reporter.summary(
              0,
              s"${successes} out of ${successes + failures + errors} tests passed.")

        TestSummary(successes, failures, errors)
      }
    }

  def runMacro[TS <: TestSuite : c.WeakTypeTag](c: WhiteboxContext)(
      ts: c.Expr[TS])(mode: c.Expr[Mode[`run`]]): c.Expr[Any] = {
    import c.universe._
    import compatibility._

    val cls = weakTypeOf[TS]
    val allMethods =
      weakTypeOf[TS].members.to[List].filter(_.isMethod).map(_.asMethod)
    val matchingMethods =
      allMethods filter { m =>
        paramLists(c)(m).isEmpty &&
        m.returnType.weak_<:<(weakTypeOf[TestSuite#Test])
      }
    val methodNames =
      matchingMethods map { m =>
        Select(ts.tree, termName(c, m.name.toString))
      }
    val listApply = Select(reify(List).tree, termName(c, "apply"))

    c.Expr(
        q"""_root_.rapture.test.run.doTests(_root_.scala.List(..$methodNames), $mode)""")
  }

  def includeMacro[TS <: TestSuite : c.WeakTypeTag](c: WhiteboxContext)(
      suite: c.Expr[TS]): c.Expr[Unit] = {
    import c.universe._
    import compatibility._

    val cls = weakTypeOf[TS]
    val allMethods =
      weakTypeOf[TS].members.to[List].filter(_.isMethod).map(_.asMethod)
    val matchingMethods =
      allMethods filter { m =>
        paramLists(c)(m).isEmpty &&
        m.returnType.weak_<:<(weakTypeOf[TestSuite#Test])
      }
    val methodNames =
      matchingMethods map { m =>
        val sel = Select(suite.tree, termName(c, m.name.toString))
        val suiteName = cls.toString.replaceAll(".type$", "").split("\\.").last
        q"""($suiteName+" / "+$sel.name, $sel)"""
      }
    val listApply = Select(reify(List).tree, termName(c, "apply"))

    c.Expr[Unit](q"""includeAll(_root_.scala.List(..$methodNames))""")
  }
}
