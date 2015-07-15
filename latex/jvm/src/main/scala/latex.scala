/******************************************************************************************************************\
* Rapture LaTeX, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                    *
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

package rapture.latex

import rapture.io._
import rapture.fs._
import rapture.text._
import rapture.core._
import rapture.cli._
import rapture.codec._, encodings.`UTF-8`._
  
trait LatexBackend {
  def process(latex: Latex, mode: Mode[_], tmp: TemporaryStorage): PdfFile
}

package latexBackends {
  object xelatex {
    implicit val implicitLatexBackend: LatexBackend = new LatexBackend {
      def process(latex: Latex, mode: Mode[_], tmp: TemporaryStorage): PdfFile = {
        val file = tmp.tmpFile(prefix = "tmp", suffix = ".tex")
        implicit val pwd: Pwd = Pwd(file.parent)
        latex.content.copyTo(file)
        val output = Process("/usr/bin/xelatex", "-interaction", "nonstopmode", file.pathString).as[String]
        val iter = output.split("\n").to[Iterator]

        // FIXME: Avoid using iterators
        while(iter.hasNext) {
          var next = iter.next()
          if(next startsWith "! ") {
            val msg = next.drop(if(next.startsWith("! LaTeX Error: ")) 15 else 2)
            val content = new StringBuilder()
            while(iter.hasNext && !next.startsWith("l.")) {
              next = iter.next()
              if(!next.startsWith("l.")) content.append(s"$next\n")
              else {
                val line = next.drop(2).takeWhile(_ != ' ').toInt
                mode.exception(LatexException(msg, line, content.toString.trim))
              }
            }
          }
        }
        val outFile = file.parent / file.filename.replaceAll("tex$", "pdf")

        PdfFile(outFile)
      }
    }
  }
}

object Latex {
  def escape(string: String): String = string.flatMap {
    case c@('#' | '$' | '%' | '&' | '_' | '{' | '}') => s"\\$c"
    case '\\' => "\\textbackslash{}"
    case '\n' => " \\\\\n"
    case '^' => "\\textasciicircum{}"
    case '~' => "\\textasciitilde{}"
    case c => c.toString
  }
}

trait `Latex#generate` extends MethodConstraint

case class LatexException(msg: String, line: Int, content: String) extends Exception {
  override def getMessage = s"latex error at line $line: $msg"
}

case class Latex(content: String) {
  def generate()(implicit backend: LatexBackend, mode: Mode[`Latex#generate`],
      tmp: TemporaryStorage): mode.Wrap[PdfFile, LatexException] =
    mode.wrap { backend.process(this, mode, tmp) }
}

case class PdfFile(file: FileUrl)

object Latexable {
  implicit val stringLatexable: Latexable[String] = new Latexable[String] {
    def toLatex(s: String) = Latex.escape(s)
  }
  
  implicit val latexLatexable: Latexable[Latex] = new Latexable[Latex] {
    def toLatex(latex: Latex) = latex.content
  }
  
  implicit def seqLatexable[T: Latexable]: Latexable[Seq[T]] = new Latexable[Seq[T]] {
    def toLatex(seq: Seq[T]) = seq.map(?[Latexable[T]].toLatex(_)).mkString
  }
}

trait Latexable[-T] { def toLatex(t: T): String }

object ToLatex {
  implicit def toLatex[T: Latexable](t: T): ToLatex = ToLatex(?[Latexable[T]].toLatex(t))
}
case class ToLatex(content: String)

object `package` {
  implicit class LatexStringContext(sc: StringContext) {
    def latex(variables: ToLatex*)(implicit process: TextProcess) =
      Latex(process(sc.parts.zip(variables.map(_.content)).map { case (a, b) => a+b }.mkString + sc.parts.last))
  }
}
