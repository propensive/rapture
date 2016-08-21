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

object TextProcess {
  implicit val defaultTextProcess: TextProcess = unindent()
}
trait TextProcess {
  def apply(s: String): String
}

object unindent {
  def apply(): TextProcess = implicitTextProcess
  implicit val implicitTextProcess: TextProcess = new TextProcess {
    def apply(s: String): String = {
      val lines = s.split("\n").dropWhile(_.isEmpty)
      val indent = lines.headOption.getOrElse("").indexWhere(_ != ' ')
      lines.map { ln =>
        if (ln.take(indent).forall(_ == ' ')) ln.drop(indent) else ln.dropWhile(_ == ' ')
      }.mkString("\n")
    }
  }
}

object smartStringOrdering {
  implicit val stringOrdering: Ordering[String] = new Ordering[String] {
    private def surrounding(s: String) = s.split("[0-9]+").to[List]
    private def numbers(s: String) = s.split("[^0-9]+").to[List].filter(_ != "").map(_.toInt)
    def compare(a: String, b: String) = {
      val as = surrounding(a)
      val bs = surrounding(b)
      val an = numbers(a)
      val bn = numbers(b)
      
      if(as != bs) math.Ordering.String.compare(a, b)
      else an.zip(bn).dropWhile { case (ai, bi) => ai == bi }.headOption.map { case (ad, bd) =>
        math.Ordering.Int.compare(ad, bd)
      }.getOrElse(0)
    }
  }
}
