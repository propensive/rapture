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

package rapture.forms

import scala.reflect.runtime.universe.{TypeTag, Type}

case class Questionnaire(questions: Vector[Question]) {
  def run(): Map[Symbol, String] = questions.foldLeft(Map[Symbol, String]()) { case (acc, q) =>
    println()
    print(q.content+" ")
    acc.updated(q.id, scala.io.StdIn.readLine)
  }
}

case class Question(id: Symbol, content: String)

object MonoidAction {
  implicit def questionsMonoidAction: MonoidAction[Question] { type Return = Questionnaire } = new MonoidAction[Question] {
    type Return = Questionnaire
    def empty: Questionnaire = Questionnaire(Vector())
    def combine(a: Questionnaire, b: Question): Questionnaire = Questionnaire(a.questions :+ b)
  }
}
trait MonoidAction[T] {
  type Return
  def empty: Return
  def combine(a: Return, b: T): Return
}

trait Widget[-W] {
  def typeValue: Type
}
case class StringInput(id: Symbol, name: String) extends Widget[StringInput] { def typeValue = implicitly[TypeTag[StringInput]].tpe }
case class Checkbox() extends Widget[Checkbox] { def typeValue = implicitly[TypeTag[Checkbox]].tpe }
case class Dropdown(id: Symbol, name: String, options: Iterable[String]) extends Widget[Dropdown] { def typeValue = implicitly[TypeTag[Dropdown]].tpe }

case class Handler[-W, +R](handlers: Vector[(Type, W => R)]) {
  def ++[W2, R2 >: R](that: Handler[W2, R2]): Handler[W with W2, R2] = Handler[W with W2, R2](handlers ++ that.handlers)
}

object on {
  def apply[W] = On[W]()
  case class On[W]() {
    def apply[R](handler: W => R)(implicit tt: TypeTag[W]): Handler[W, R] = Handler(Vector(tt.tpe -> handler))
  }
}

case class Form[W](widgets: Widget[W]*) {
  def render[W2 <: W, Return](renderers: Handler[W2, Return]*)(implicit monoidAction: MonoidAction[Return]): monoidAction.Return = {
    val handlers = renderers.map(_.handlers).reduce(_ ++ _)
    widgets.map { w =>

      val fn = handlers.find(_._1 <:< w.typeValue).get._2.asInstanceOf[Widget[W] => Return]
      fn(w)
    }.foldLeft(monoidAction.empty)(monoidAction.combine(_, _))
  }
}

object Testing {

  val form: Form[StringInput with Dropdown] = Form(
    StringInput('firstName, "First name"),
    StringInput('surname, "Last name"),
    Dropdown(id = 'dropdown, "What do you think?", List("Yes", "No", "Maybe")),
    Dropdown(id = 'dropdown, "What do you thing?", (1 to 10).map(_.toString))
  )

  val q: Questionnaire = form.render(
    on[StringInput] { si => Question(si.id, "What's your name?") },
    on[Dropdown] { dd => Question(dd.id, "Do you like it? "+dd.options.mkString("/")) }
  )

}
