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

package rapture.http

import scala.language.higherKinds

import rapture.core._
import rapture.mime._
import rapture.uri._
import rapture.net._
import rapture.dom._
import rapture.html._
import rapture.css._

import scala.collection.mutable.ListBuffer

object Forms extends Widgets with Parsers {

  class BasicForm(val name: Symbol,
                  val params: Map[String, String] = Map(),
                  val uploads: Map[String, Array[Byte]] = Map()) { form =>

    type Field[T] <: BasicField[T]

    val formName = name.name
    protected val fields = new ListBuffer[Field[_]]

    def submitted = params.contains(formName + "_submit")
    def complete = submitted
    def save() = fields.foreach(_.save())

    trait BasicField[T] {
      def name: Symbol
      def fieldName: String = name.name
      def paramValue: Option[String] = form.params.get(fieldName)

      def value: Option[T] =
        if (parser.submitted(stringValue)) Some(parser.parse(stringValue, dataValue)) else None

      def dataValue: Option[Array[Byte]] = form.uploads.get(fieldName)

      def stringValue: Option[String] =
        if (form.submitted) paramValue else if (cell == null) None else parser.serialize(cell())

      def fieldValue: String = stringValue.getOrElse("")
      def apply(): T = value.get
      def parser: FieldParser[T]
      def cell: Cell[T]
      def save(): Unit = value foreach { c =>
        if (cell == null) () else cell.update(c)
      }

      override def toString = s"${fieldName}: value=${value}"
    }
  }

  trait FieldLabels {
    type FormField[T] <: LabelledField
    trait LabelledField { def label: String }
  }

  trait FormValidation {
    this: (BasicForm with FormValidation) =>

    def validated = fields.forall(_.validated)
    def showValidation = submitted && !validated

    abstract override def complete = submitted && validated

    type Field[T] <: ValidatedField[T]
    trait ValidatedField[T] {
      this: BasicField[T] =>
      lazy val validationIssues: List[String] = if (!submitted) Nil else validator(stringValue)
      def validated: Boolean = validationIssues.isEmpty
      def validator: Option[String] => List[String]
      def required: Boolean

      override def toString =
        s"${fieldName}: value=${value}, validated=${validated}, required=${required}, validator=${validator}, parser=${parser}, cell=${cell}, paramValue=${paramValue}"
    }

    // String validators

    // FIXME: Reformat these lines
    val validUrl: Option[String] => List[String] = {
      case None => Nil
      case Some(s) =>
        if (s.matches("\\b(https?|ftp)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#" +
                  "/%=~_|]")) Nil
        else List("Please enter a valid URL")
    }

    val validInteger: Option[String] => List[String] = {
      case None => Nil
      case Some(s) => if (s.matches("^-?[1-9][0-9]*$")) Nil else List("Please enter a valid number")
    }

    val validPhoneNumber: Option[String] => List[String] = {
      case Some(s) =>
        if (s.matches("""^[+\- ()0-9]*$""")) Nil
        else
          List(
              "Please enter a valid tele" +
                "phone number")
      case None => Nil
    }

    val validTwitterHandle: Option[String] => List[String] = {
      case Some(s) =>
        if (s.matches("|[a-zA-Z0-9_]{1,15}")) Nil
        else List("Please enter a valid Twitter handle, or leave this field blank.")
      case None => Nil
    }

    val validEmailAddress: Option[String] => List[String] = {
      case Some(s) =>
        if (s.matches("""^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2}[a-z]*)$""")) Nil
        else List("Please enter a valid email address")
      case None => Nil
    }

    val optValidEmailAddress: Option[String] => List[String] = {
      case Some(s) =>
        if (s.matches("""^([_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-"+
        "9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4}))?$""")) Nil
        else
          List(
              "Please enter a valid email addre" +
                "ss")
      case None => Nil
    }

    val validDateTime: Option[String] => List[String] = {
      case Some(s) =>
        if (s.matches("[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9] [0-9][0-" +
                  "9]:[0-9][0-9]:[0-9][0-9]")) Nil
        else
          List(
              "Please enter a valid date, in the format DD/MM" +
                "/YY hh:mm:ss.")
      case None => Nil
    }

    def notEmpty(msg: String = "Value is required and can't be empty."): Option[String] => List[String] = {
      case Some(s) => if (s.isEmpty) List(msg) else Nil
      case None => Nil
    }

    def minimumLength(length: Int): Option[String] => List[String] = {
      case Some(s) => if (s.length < 8) List(s"The password must be at least $length characters long") else Nil
      case None => Nil
    }

    val isSlug: Option[String] => List[String] = {
      case Some(s) =>
        if (!s.matches("[a-z0-9]*"))
          List(
              "Value can only contain lower-case alphanum" +
                "eric characters.")
        else Nil
      case None => Nil
    }

    val isChecked: Option[String] => List[String] = {
      case Some(s) => Nil
      case None => List("You must check this field to continue.")
    }

    def notDuplicate(xs: List[String]): Option[String] => List[String] = {
      case Some(s) =>
        if (xs contains s)
          List(
              "This value is not unique. Please choose something di" +
                "fferent.")
        else Nil
      case None => Nil
    }
  }

  trait FormHelp {
    this: BasicForm =>
    type Field[T] <: HelpField[T]
    trait HelpField[T] extends BasicField[T] { def help: String }
  }

  trait Preprocessing {
    this: BasicForm =>

    type Field[T] <: PreprocessedField[T]

    trait PreprocessedField[T] extends BasicField[T] {
      def processString(s: String): String
      override def stringValue = super.stringValue.map(processString)
    }
  }

  /** Adds renderability functionality to a form.  */
  trait RenderableForm {
    this: (BasicForm with RenderableForm) =>
    type Field[T] <: RenderableField[T]
    type RenderType
    type FormPart
    type RenderedForm
    val formParts = new ListBuffer[FormPart]

    def wrap[T, F <: Field[T], W <: Widget](field: F, widget: W)(implicit renderer: Renderer[T, F, W]): FormPart

    def content(fp: FormPart) = formParts += fp

    def render: RenderedForm

    // asInstanceOf[F] is here as an indirect consequence of compiler bug SI-6443
    trait RenderableField[T] {
      this: Field[T] =>
      def as[F <: Field[T], W <: Widget](w: W)(implicit renderer: Renderer[T, F, W]): this.type = {
        formParts += wrap[T, F, W](this.asInstanceOf[F], w)(renderer)
        fields += this
        this
      }
    }

    trait Renderer[T, -F <: RenderableField[T], -W <: Widget] {
      def render(f: F, w: W): RenderType
      def hideLabel: Boolean = false
    }
  }

  object WebForm {
    implicit class FormExtras[F <: Forms.WebForm[_]](f: F) {
      def show[T: HttpHandler](p1: F => T) = new {
        def andThen[S: HttpHandler](p2: F => S): Response =
          if (f.complete) {
            f.save()
            ?[HttpHandler[S]].response(p2(f))
          } else ?[HttpHandler[T]].response(p1(f))
      }
    }
  }

  abstract class WebForm[L](name: Symbol,
                            params: Map[String, String] = Map(),
                            uploads: Map[String, Array[Byte]] = Map(),
                            val postMethod: HttpMethods.FormMethod = HttpMethods.Post,
                            val formAction: L = ^)(implicit actionLinkableParam: Linkable[L])
      extends BasicForm(name, params, uploads)
      with RenderableForm
      with FieldLabels
      with Preprocessing
      with FormValidation
      with FormHelp {

    implicit protected def actionLinkable: Linkable[L] = actionLinkableParam

    def encoding: MimeTypes.MimeType =
      if (fields.exists(_.needsMultipart)) MimeTypes.`multipart/form-data`
      else MimeTypes.`application/x-www-form-urlencoded`

    class Field[T](val name: Symbol,
                   val label: String,
                   val cell: Cell[T],
                   val parser: FieldParser[T],
                   process: String => String,
                   validate: Option[String] => List[String],
                   val required: Boolean,
                   val help: String,
                   val needsMultipart: Boolean = false)
        extends BasicField[T]
        with RenderableField[T]
        with LabelledField
        with PreprocessedField[T]
        with ValidatedField[T]
        with HelpField[T] {
      def processString(s: String) = process(s)
      def validator = validate
    }

    def field[T: FieldParser](name: Symbol,
                              label: String,
                              cell: Cell[T] = null,
                              process: (String => String) = identity[String],
                              validate: Option[String] => List[String] = { s =>
                                Nil
                              },
                              required: Boolean = false,
                              help: String = "") =
      new Field[T](name,
                   label,
                   cell,
                   ?[FieldParser[T]],
                   process,
                   validate,
                   required,
                   help,
                   ?[FieldParser[T]].needsMultipart)

    import htmlSyntax._

    type RenderType = DomNode[_ <: ElementType, Html5.Phrasing, _ <: AttributeType]

    implicit val stringRenderer = new Renderer[String, Field[String], StringInput] {
      def render(f: Field[String], w: StringInput): RenderType =
        Input(htmlSyntax.name = f.name, typ = "text", value = f.fieldValue)
    }

    implicit val passwordRenderer = new Renderer[String, Field[String], PasswordInput] {
      def render(f: Field[String], w: PasswordInput): RenderType =
        Input(htmlSyntax.name = f.name, typ = "password", value = f.fieldValue)
    }

    implicit val uploadRenderer = new Renderer[Array[Byte], Field[Array[Byte]], FileUploader] {
      def render(f: Field[Array[Byte]], w: FileUploader): RenderType =
        Input(htmlSyntax.name = f.name, typ = "file", value = f.fieldValue)
    }

    implicit val checkboxRenderer = new Renderer[Boolean, Field[Boolean], Checkbox] {
      override def hideLabel = true
      def render(f: Field[Boolean], w: Checkbox): RenderType =
        Label(
            Input(typ = "checkbox",
                  htmlSyntax.value = "1",
                  htmlSyntax.name = f.name,
                  checked = f.value.getOrElse(false)),
            " " + f.label
        )
    }

    implicit val textareaRenderer = new Renderer[String, Field[String], TextArea] {
      def render(f: Field[String], w: TextArea): RenderType =
        Textarea(htmlSyntax.name = f.name, maxlength = w.maxLength.getOrElse(-1))(TextNode(f.fieldValue))
    }

    implicit def dropdownRenderer[T, Q] = new Renderer[T, Field[T], Dropdown[Q]] {
      def render(f: Field[T], w: Dropdown[Q]): RenderType =
        Select(htmlSyntax.name = f.name)(
            w.options.map { opt =>
              Option(value = w.id(opt))(TextNode(w.description(opt)))
            }: _*
        )
    }

    implicit def radioListRenderer[T, Q] = new Renderer[T, Field[T], RadioList[Q]] {
      def render(f: Field[T], w: RadioList[Q]): RenderType =
        Span(style = css"display:inline-block")(
            w.options.flatMap { opt =>
              List(
                  Span(
                      Input(typ = "radio",
                            htmlSyntax.name = f.name,
                            value = w.id(opt),
                            checked = w.id(opt) == f.fieldValue),
                      " " + w.description(opt),
                      Br
                  )
              )
            }: _*
        )
    }

    implicit val hiddenRenderer = new Renderer[String, Field[String], Hidden] {
      def render(f: Field[String], w: Hidden): RenderType =
        Input(typ = "hidden", htmlSyntax.name = f.name, htmlSyntax.value = f.fieldValue)
    }
  }

  class BootstrapForm[L: Linkable](name: Symbol,
                                   params: Map[String, String],
                                   uploads: Map[String, Array[Byte]] = Map(),
                                   postMethod: HttpMethods.FormMethod = HttpMethods.Post,
                                   formAction: L = ^)
      extends WebForm[L](name, params, uploads, postMethod, formAction)
      with FormValidation {
    import htmlSyntax._

    type FormPart = DomNode[_ <: ElementType, Html5.Flow, _ <: AttributeType]
    type RenderedForm = HtmlElement[Html5.Flow]

    def hideLabels = false

    def wrap[T, F <: Field[T], W <: Widget](field: F, widget: W)(implicit renderer: Renderer[T, F, W]): FormPart =
      Div(cls = List.strap("control-group", field.validationIssues.headOption.map(x => "error")))(
          Label(cls = "control-label" /*, forName = field.name.name*/ )(TextNode(field.label)),
          Div(cls = 'controls)(
              renderer.render(field, widget),
              Div(
                  Div,
                  field.validationIssues.map { i =>
                    Span(cls = "help-inline")(i + " ")
                  }: _*
              )
          )
      )

    def render: RenderedForm =
      Form(enctype = encoding.toString, cls = 'form, action = formAction, method = postMethod.toString)(
          Fieldset(
              formParts.head,
              List.strap(
                  formParts.tail.toList,
                  submitRow
              ): _*
          )
      )

    def submitRow =
      Div(
          Input(htmlSyntax.name = Symbol(formName + "_submit"),
                typ = "submit",
                cls = List("btn", "btn-primary"),
                value = submitButtonText))

    def submitButtonText = "Save"
  }

  trait TabularLayout[L] {
    this: (WebForm[L] with TabularLayout[L]) =>

    import htmlSyntax._

    type FormPart = List[HtmlElement[Html5.TrItems]]
    type RenderedForm = HtmlElement[Html5.Flow]

    def hideLabels = false

    def wrap[T, F <: Field[T], W <: Widget](field: F, widget: W)(implicit renderer: Renderer[T, F, W]): FormPart = {
      List.strap(
          field.validationIssues.map { err =>
            Tr(Td(colspan = 3)(""), Td(cls = 'validation)(err))
          },
          Tr(Td,
             List.strap(
                 if (hideLabels) Nil
                 else if (renderer.hideLabel) List(Td(""), Td(""))
                 else
                   List(
                       Td(field.label),
                       Td(if (field.required) "*" else "")
                   ),
                 Td(renderer.render(field, widget))
             ): _*)
      )
    }

    def render: RenderedForm = {
      val fps = formParts.flatten
      Form(enctype = encoding.toString, action = formAction, method = postMethod.toString)(
          Table(
              Tbody(
                  fps.head,
                  List.strap(
                      fps.tail.toList,
                      submitRow
                  ): _*
              )
          )
      )
    }

    def submitButtonText = "Save"

    def submitRow =
      if (hideLabels) Tr(Td, Td(submitButton))
      else
        Tr(
            Td,
            Td,
            Td,
            Td(submitButton)
        )

    def submitButton: HtmlElement[Html5.Flow] =
      Input(htmlSyntax.name = Symbol(formName + "_submit"), value = submitButtonText, typ = "submit")

  }
}

trait Parsers {
  @annotation.implicitNotFound(
      "Unable to use values of type ${Value} in form fields without a corresponding FieldParser.")
  trait FieldParser[Value] {
    def parse(value: Option[String], data: Option[Array[Byte]] = None): Value
    def serialize(value: Value): Option[String]
    def submitted(value: Option[String]): Boolean = value.isDefined
    def needsMultipart: Boolean = false
  }

  implicit val StringParser = new FieldParser[String] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None) = s.getOrElse("")
    def serialize(s: String): Option[String] = Some(s)
  }

  implicit val IntParser = new FieldParser[Int] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None): Int = s.get.toInt
    def serialize(value: Int) = Some(value.toString)
  }

  implicit val BooleanParser = new FieldParser[Boolean] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None) = s.isDefined
    def serialize(value: Boolean) = if (value) Some("") else None
    override def submitted(value: Option[String]): Boolean = true
  }

  implicit val DataParser = new FieldParser[Array[Byte]] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None) = data.getOrElse(Array[Byte]())
    def serialize(value: Array[Byte]) = Some("")
    override def needsMultipart: Boolean = true
  }

  def enumParser(enum: Enumeration) = new FieldParser[enum.Value] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None) = enum(s.get.toInt)
    def serialize(value: enum.Value) = Some(value.id.toString)
  }

}
