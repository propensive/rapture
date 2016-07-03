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

package rapture.latex

import rapture.io._
import rapture.fs._
import rapture.text._
import rapture.uri._
import rapture.core._
import rapture.cli._
import rapture.codec._, encodings.`UTF-8`._

import language.implicitConversions

object TemporaryStorage {
  implicit def defaultTemporary: TemporaryStorage = TemporaryStorage(uri"file:///tmp")
}

case class TemporaryStorage(file: FsUrl) {
  def tmpFile(prefix: String = "rapture", suffix: String = ".tmp") =
    File.parse(java.io.File.createTempFile(prefix, suffix, file.javaFile).getAbsolutePath)
}

trait LatexBackend {
  def process(latex: Latex, data: Seq[(String, Bytes)], mode: Mode[_], tmp: TemporaryStorage): PdfFile
}

package latexBackends {
  object xelatex {
    implicit val implicitLatexBackend: LatexBackend = new LatexBackend {
      def process(latex: Latex, data: Seq[(String, Bytes)], mode: Mode[_], tmp: TemporaryStorage): PdfFile = {
        val dir = tmp.file / Guid.generate()()
        dir.mkdir()
        data.foreach { case (name, bytes) =>
          val f = dir / name
          bytes.copyTo(f)
        }
        val file = dir / "document.tex"
        implicit val env: Environment = new Environment {
          def workDir = Some(dir.javaFile.getAbsolutePath)
          def apply() = environments.enclosing()()
        }

        latex.content.copyTo(file)
        val cmd = sh"/usr/bin/xelatex -interaction nonstopmode ${file}"
        val output = cmd.exec[Iterator[String]]

        while(output.hasNext) {
          var next = output.next()
          if(next startsWith "! ") {
            val msg = next.drop(if(next.startsWith("! LaTeX Error: ")) 15 else 2)
            val content = new StringBuilder()
            while(output.hasNext && !next.startsWith("l.")) {
              next = output.next()
              if(!next.startsWith("l.")) content.append(s"$next\n")
              else {
                val line = next.drop(2).takeWhile(_ != ' ').toInt
                mode.exception(LatexException(msg, line, content.toString.trim))
              }
            }
          }
        }

        PdfFile((file.parent / file.filename.replaceAll("tex$", "pdf")).slurp[Byte])
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

  def generate(data: (String, Bytes)*)(implicit backend: LatexBackend, mode: Mode[`Latex#generate`],
      tmp: TemporaryStorage): mode.Wrap[PdfFile, LatexException] =
    mode.wrap { backend.process(this, data, mode, tmp) }
}

case class PdfFile(data: Bytes)

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

case class LatexStringContext(sc: StringContext) {
  def latex(variables: Annex[Latexable]*)(implicit process: TextProcess) =
    Latex(process(sc.parts.zip(variables.map(_(_.toLatex))).map { case (a, b) => a+b }.mkString + sc.parts.last))
}

object `package` {
  implicit def latexStringContext(sc: StringContext): LatexStringContext = LatexStringContext(sc)
}
