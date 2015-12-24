/******************************************************************************************************************\
* Rapture HTML, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                     *
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
package rapture.html

import rapture.dom._
import rapture.core._
import rapture.codec._
import rapture.net._

import language.dynamics

object htmlSyntax {

  import Html5._

  implicit def stringToTextNode(str: String):
      TextNode[Nothing, Nothing, Html5.Text] =
    TextNode[Nothing, Nothing, Html5.Text](str)

  type HtmlRoot = Element[Top, ElementType, Html]
  
  type HtmlElement[T <: ElementType] = Element[_ <: ElementType, _ <: T, _ <: AttributeType]

  // FIXME: These tag types are not currently working
  def A[T <: ElementType] = Tag[T, T, A](block = false)(new AssignedName("A"))
  def Ins[T <: ElementType] = Tag[T, T, Edit]()(new AssignedName("Ins"))
  def Del[T <: ElementType] = Tag[T, T, Edit]()(new AssignedName("Del"))
  def Object[T <: Embedded] = Tag[T, T, Object]()(new AssignedName("Object"))
  def Video[T <: Embedded with Interactive] = Tag[T, T, Video]()(new AssignedName("Video"))
  def Audio[T <: Embedded with Interactive] = Tag[T, T, Audio]()(new AssignedName("Audio"))
  def Canvas[T <: Embedded] = Tag[T, T, Canvas]()(new AssignedName("Canvas"))
 
  val Html = Tag[Top, ElementType, Html]()
  val Head = Tag[Metadata, Top, Nothing]()
  val Title = Tag[Text, Metadata, Global]()
  val Base = Tag[Nothing, Metadata, Base]()
  val Link = Tag[Nothing, Metadata, Link]()
  val Meta = Tag[Metadata, Metadata, Meta]()
  val Style = Tag[Text, Metadata with Flow, Global](block = true)
  val Script = Tag[Text, Metadata with Phrasing, Script](forceClosingTag = true, block = true)
  val Noscript = Tag[Text, Metadata with Phrasing, AttributeType]()
  val Body = Tag[Flow, Top, Body]()
  val Section = Tag[Flow, Sectioning, AttributeType]()
  val Nav = Tag[Flow, Sectioning, AttributeType]()
  val Article = Tag[Flow, Sectioning, AttributeType]()
  val Aside = Tag[Flow, Sectioning, AttributeType]()
  val H1 = Tag[Phrasing, Heading, AttributeType]()
  val H2 = Tag[Phrasing, Heading, AttributeType]()
  val H3 = Tag[Phrasing, Heading, AttributeType]()
  val H4 = Tag[Phrasing, Heading, AttributeType]()
  val H5 = Tag[Phrasing, Heading, AttributeType]()
  val H6 = Tag[Phrasing, Heading, AttributeType]()
  val Header = Tag[Flow, Heading, AttributeType]()
  val Footer = Tag[Flow, Flow, AttributeType]()
  val Address = Tag[Flow, Flow, AttributeType]()
  val P = Tag[Phrasing, Flow, AttributeType]()
  val Hr = Tag[Nothing, Flow, AttributeType]()
  val Br = Tag[Phrasing, Nothing, AttributeType](block = false)
  val Pre = Tag[Phrasing, Flow, AttributeType]()
  val Dialog = Tag[Definitions, Flow, AttributeType]()
  val Blockquote = Tag[Flow, Sectioning, Blockquote]()
  val Ol = Tag[ListItems, Flow, Ol]()
  val Ul = Tag[ListItems, Flow, AttributeType]()
  val Li = Tag[Flow, ListItems, Li]()
  val Dl = Tag[Definitions, Flow, AttributeType]()
  val Dt = Tag[Phrasing, Definitions, AttributeType]()
  val Dd = Tag[Flow, Definitions, AttributeType]()
  val Q = Tag[Phrasing, Phrasing, AttributeType]()
  val Cite = Tag[Phrasing, Phrasing, AttributeType]()
  val Em = Tag[Phrasing, Phrasing, AttributeType]()
  val Strong = Tag[Phrasing, Phrasing, AttributeType]()
  val Small = Tag[Phrasing, Phrasing, AttributeType]()
  val Mark = Tag[Phrasing, Phrasing, AttributeType]()
  val Dfn = Tag[Phrasing, Phrasing, AttributeType]()
  val Time = Tag[Phrasing, Phrasing, AttributeType]()
  val Progress = Tag[Phrasing, Phrasing, AttributeType]()
  val Meter = Tag[Phrasing, Phrasing, Meter]()
  val Code = Tag[Phrasing, Phrasing, AttributeType]()
  val Var = Tag[Phrasing, Phrasing, AttributeType]()
  val Samp = Tag[Phrasing, Phrasing, AttributeType]()
  val Kbd = Tag[Phrasing, Phrasing, AttributeType]()
  val Sup = Tag[Phrasing, Phrasing, AttributeType](block = false)
  val Sub = Tag[Phrasing, Phrasing, AttributeType](block = false)
  val Span = Tag[Phrasing, Phrasing, AttributeType](block = false)
  val I = Tag[Phrasing, Phrasing, AttributeType](block = false)
  val B = Tag[Phrasing, Phrasing, AttributeType](block = false)
  val Bdo = Tag[Phrasing, Phrasing, AttributeType]()
  val Ruby = Tag[Phrasing, Phrasing, AttributeType]()
  val Rt = Tag[Nothing, Phrasing, AttributeType]()
  val Rp = Tag[Nothing, Phrasing, AttributeType]()
  val Figure = Tag[Flow, Sectioning, AttributeType]()
  val Img = Tag[Nothing, Embedded, Img]()
  val Iframe = Tag[Text, Embedded, Iframe]()
  val Embed = Tag[Text, Embedded, Embed]()
  val Param = Tag[Flow, Embedded, Param]()
  val Source = Tag[Nothing, Flow, Source]()
  val Map = Tag[Flow, Flow, Map]()
  val Area = Tag[Nothing, Phrasing, Area]()
  val Table = Tag[TableItems, Flow, Area]()
  val Caption = Tag[Phrasing, TableItems, AttributeType]()
  val Colgroup = Tag[ColItems, TableItems, Col]()
  val Col = Tag[Nothing, ColItems, Col]()
  val Tbody = Tag[TrItems, TableItems, AttributeType]()
  val Thead = Tag[TrItems, TableItems, AttributeType]()
  val Tfoot = Tag[TrItems, TableItems, AttributeType]()
  val Tr = Tag[TdItems, TrItems, AttributeType]()
  val Td = Tag[Flow, TdItems, Td]()
  val Th = Tag[Flow, TdItems, Th]()
  val Form = Tag[Flow, Flow, Form]()
  val Fieldset = Tag[Flow, Flow, Fieldset]()
  val Label = Tag[Phrasing, Phrasing with Interactive, Label]()
  val Input = Tag[Nothing, Phrasing with Interactive, Input]()
  val Button = Tag[Nothing, Phrasing with Interactive, Button]()
  val Select = Tag[OptionItems, Phrasing with Interactive, Select]()
  val Datalist = Tag[OptionItems, Phrasing with Interactive, AttributeType]()
  val Optgroup = Tag[OptionItems, Phrasing, Optgroup]()
  val Option = Tag[Text, OptionItems, Option]()
  val Textarea = Tag[Text, Phrasing with Interactive, Textarea](forceClosingTag = true)
  val Output = Tag[Phrasing, Phrasing, Output]()
  val Details = Tag[Flow, Interactive, Details]()
  val Command = Tag[Nothing, Metadata with Phrasing, Command]()
  val Bb = Tag[Phrasing, Phrasing with Interactive, Bb]()
  val Menu = Tag[ListItems, Phrasing with Interactive, Menu]()
  val Legend = Tag[Phrasing, ElementType, AttributeType]()
  val Div = Tag[Flow, Flow, AttributeType](forceClosingTag = true)

  object data extends Dynamic {
    def selectDynamic(att: String) = Attribute[Global, String](att)(identity)
    def updateDynamic[E <: ElementType](att: String)(v: String) = selectDynamic(att).set[E](v)
  }

  implicit def id = Attribute[Global, Symbol]("id")(_.name)
  def id_=[E <: ElementType](v: Symbol) = id.set[E](v)
  
  implicit def lang = Attribute[Global, Symbol]("lang")(_.name)
  def lang_=[E <: ElementType](v: Symbol) = lang.set[E](v)
  
  implicit def translate = Attribute[Global, Boolean]("translate")(v => if(v) "yes" else "no")
  def translate_=[E <: ElementType](v: Boolean) = translate.set[E](v)
  
  implicit def classes = Attribute[Global, Seq[String]]("classes", "class")(_.mkString(" "))
  def classes_=[E <: ElementType](v: Seq[String]) = classes.set[E](v)
  
  implicit def onload = Attribute[Body, String]("onload")(identity)
  def onload_=[E <: ElementType](v: String) = onload.set[E](v)
  
  implicit def onclick = Attribute[Global, String]("onclick")(identity)
  def onclick_=[E <: ElementType](v: String) = onclick.set[E](v)
  
  implicit def title = Attribute[Global, String]("title")(identity)
  def title_=[E <: ElementType](v: String) = title.set[E](v)
  
  implicit def alt = Attribute[Img with Area with Input, String]("alt")(identity)
  def alt_=[E <: ElementType](v: String) = alt.set[E](v)
  
  implicit def href = Attribute[Base with Link with A with Area, rapture.uri.Link]("href")(_.toString)
  def href_=[E <: ElementType](v: rapture.uri.Link) = href.set[E](v)
  
  implicit def name = Attribute[Meta with Iframe with Object with Param with Map with Form with Fieldset
      with Input with Button with Select with Textarea with Output, Symbol]("name")(_.name)
  def name_=[E <: ElementType](v: Symbol) = name.set[E](v)

  implicit def selected = Attribute[Option, Boolean]("selected") { v => if(v) "selected" else null }
  def selected_=[E <: ElementType](v: Boolean) = selected.set[E](v)
  
  implicit def cols = Attribute[Textarea, Int]("cols")(_.toString)
  def cols_=[E <: ElementType](v: Int) = cols.set[E](v)
  
  implicit def rows = Attribute[Textarea, Int]("rows")(_.toString)
  def rows_=[E <: ElementType](v: Int) = rows.set[E](v)
  
  implicit def colspan = Attribute[Td with Th, Int]("colspan")(_.toString)
  def colspan_=[E <: ElementType](v: Int) = colspan.set[E](v)
  
  implicit def rowspan = Attribute[Td with Th, Int]("rowspan")(_.toString)
  def rowspan_=[E <: ElementType](v: Int) = rowspan.set[E](v)
  
  implicit def wrap = Attribute[Textarea, Boolean]("wrap") { v => if(v) "wrap" else null }
  def wrap_=[E <: ElementType](v: Boolean) = wrap.set[E](v)
  
  implicit def open = Attribute[Details, Boolean]("open") { v => if(v) "open" else null }
  def open_=[E <: ElementType](v: Boolean) = open.set[E](v)
  
  implicit def max = Attribute[Progress with Meter with Input, Double]("max")(_.toString)
  def max_=[E <: ElementType](v: Double) = max.set[E](v)
  
  implicit def min = Attribute[Meter with Input, Double]("min")(_.toString)
  def min_=[E <: ElementType](v: Double) = min.set[E](v)
  
  implicit def low = Attribute[Meter, Double]("low")(_.toString)
  def low_=[E <: ElementType](v: Double) = low.set[E](v)
  
  implicit def high = Attribute[Meter, Double]("high")(_.toString)
  def high_=[E <: ElementType](v: Double) = high.set[E](v)
  
  implicit def optimum = Attribute[Meter, Double]("optimum")(_.toString)
  def optimum_=[E <: ElementType](v: Double) = optimum.set[E](v)
  
  implicit def span = Attribute[Col, Int]("span")(_.toString)
  def span_=[E <: ElementType](v: Int) = span.set[E](v)
  
  class HttpEquiv(val name: String)
  case object contentType extends HttpEquiv("content-type")
  case object defaultStyle extends HttpEquiv("default-style")
  case object refresh extends HttpEquiv("refresh")
  implicit def httpEquiv = Attribute[Meta, HttpEquiv]("httpEquiv", "http-equiv")(_.name)
  def httpEquiv_=[E <: ElementType](v: HttpEquiv) = httpEquiv.set[E](v)
  
  implicit def charset = Attribute[Meta with Script, Encoding]("charset")(_.name)
  def charset_=[E <: ElementType](v: Encoding) = charset.set[E](v)
  
  implicit def content = Attribute[Meta, String]("content")(identity)
  def content_=[E <: ElementType](v: String) = content.set[E](v)
 
  implicit def manifest = Attribute[Html, HttpUrl]("manifest")(_.toString)
  def manifest_=[E <: ElementType](v: HttpUrl) = manifest.set[E](v)

  implicit def target =
    Attribute[Base with Link with A with Area with Form with Input with Button, Symbol]("target")(_.name)
  def target_=[E <: ElementType](v: Symbol) = target.set[E](v)

  sealed class Rel(val name: String)
  case object alternate extends Rel("alternate")
  case object author extends Rel("author")
  case object bookmark extends Rel("bookmark")
  case object help extends Rel("help")
  case object license extends Rel("license")
  case object next extends Rel("next")
  case object nofollow extends Rel("nofollow")
  case object noreferrer extends Rel("noreferrer")
  case object prefetch extends Rel("prefetch")
  case object prev extends Rel("prev")
  case object search extends Rel("search")
  case object stylesheet extends Rel("stylesheet")
  case object tag extends Rel("tag")
  implicit def rel = Attribute[Link with A with Area, Rel]("rel")(_.name)
  def rel_=[E <: ElementType](v: Rel) = rel.set[E](v)
 
  // FIXME: Provide &, | and ! operators to write media expressions, and implement all values
  trait MediaExpr
  sealed class Media(val name: String) extends MediaExpr { override def toString = name }
  sealed class Device(val name: String) extends MediaExpr { override def toString = name }
  case object all extends Device("all")
  case object aural extends Device("aural")
  case object braille extends Device("braille")
  case object handheld extends Device("handheld")
  case object projection extends Device("projection")
  case object print extends Device("print")
  case object screen extends Device("screen")
  case object tty extends Device("tty")
  case object tv extends Device("tv")
  implicit def media = Attribute[Link with Style with A with Source with Area, MediaExpr]("media")(_.toString)
  def media_=[E <: ElementType](v: MediaExpr) = media.set[E](v)

  implicit def style = Attribute[Global, String]("style")(identity)
  def style_=[E <: ElementType](v: String) = style.set[E](v)


  implicit def src = Attribute[Script with Img with Iframe with Embed with Video with Audio with Source with Input,
      rapture.uri.Link]("src")(_.toString)
  def src_=[E <: ElementType](v: rapture.uri.Link) = src.set[E](v)

  implicit def value = Attribute[Li with Progress with Meter with Param with Input with Button with Option, String](
      "value")(identity)
  def value_=[E <: ElementType](v: String) = value.set[E](v)
  
  
  implicit def typ = Attribute[Link with Style with Script with A with Embed with Object with Source with Area with
      Input with Button with Command with Bb with Menu, String]("typ", "type")(identity)
  def typ_=[E <: ElementType](v: String) = typ.set[E](v)

  implicit def action = Attribute[Form with Input with Button, rapture.uri.Link]("action")(_.toString)
  def action_=[E <: ElementType](v: rapture.uri.Link) = action.set(v)
  
  implicit def method = Attribute[Form with Input with Button, String]("method")(identity)
  def method_=[E <: ElementType](v: String) = method.set(v)
  
  implicit def enctype = Attribute[Form with Input with Button, String]("enctype")(identity)
  def enctype_=[E <: ElementType](v: String) = enctype.set(v)
  
  implicit def checked = Attribute[Input, Boolean]("checked")(v => if(v) "checked" else null)
  def checked_=[E <: ElementType](v: Boolean) = checked.set(v)
  
  implicit def maxlength = Attribute[Input with Textarea, Int]("maxlength")(_.toString)
  def maxlength_=[E <: ElementType](v: Int) = maxlength.set(v)
  
  /* 

  private type HreflangAttribute = Link with A with Area
  implicit def hreflang = attribute()
  def hreflang_=(value: String): Att[HreflangAttribute] = Attribute(value)

  implicit def sizes = attribute()
  def sizes_=(value: String): Att[Link] = Attribute(value)
  
  implicit def scoped = attribute()
  def scoped_=(value: String): Att[Script] = Attribute(value)
  
  implicit def async = attribute()
  def async_=(value: String): Att[Script] = Attribute(value)

  implicit def defer = attribute()
  def defer_=(value: String): Att[Script] = Attribute(value)

  implicit def onbeforeunload = attribute()
  def onbeforeunload_=(value: String): Att[Body] = Attribute(value.toString)
  
  implicit def onerror = attribute()
  def onerror_=(value: String): Att[Body] = Attribute(value)
  
  implicit def onhashchange = attribute()
  def onhashchange_=(value: String): Att[Body] = Attribute(value)
  
  implicit def onmessage = attribute()
  def onmessage_=(value: String): Att[Body] = Attribute(value)
  
  implicit def onoffline = attribute()
  def onoffline_=(value: String): Att[Body] = Attribute(value)
  
  implicit def onpopstate = attribute()
  def onpopstate_=(value: String): Att[Body] = Attribute(value)
  
  implicit def onresize = attribute()
  def onresize_=(value: String): Att[Body] = Attribute(value)
  
  implicit def onstorage = attribute()
  def onstorage_=(value: String): Att[Body] = Attribute(value)
  
  implicit def onunload = attribute()
  def onunload_=(value: String): Att[Body] = Attribute(value)
  
  implicit def reversed = attribute()
  def reversed_=(value: String): Att[Ol] = Attribute(value)
  
  implicit def start = attribute()
  def start_=(value: Int): Att[Ol] = Attribute(value.toString)
  
  implicit def ping = attribute()
  def ping_=(value: String): Att[A with Area] = Attribute(value)
  
  implicit def cite = attribute()
  def cite_=(value: String): Att[Blockquote with Q with Edit] = Attribute(value)
  
  implicit def datetime = attribute()
  def datetime_=(value: String): Att[Time with Edit] = Attribute(value)
  
  implicit def dir = attribute()
  def dir_=(value: String): Att[Global] = Attribute(value)
  
  implicit def alt = attribute()
  def alt_=(value: String): Att[Img with Area with Input] = Attribute(value)
  
  implicit def usemap = attribute()
  def usemap_=(value: Symbol): Att[Img with Object] = Attribute(value.name)
  
  implicit def ismap = attribute()
  def ismap_=(value: String): Att[Img] = Attribute(value)
  
  private type WidthAttribute = Img with Iframe with Embed with Object with Video with Canvas with Input
  implicit def width = attribute()
  def width_=(value: Int): Att[WidthAttribute] = Attribute(value.toString)
  
  implicit def height = attribute()
  def height_=(value: Int): Att[WidthAttribute] = Attribute(value.toString)
  
  implicit def sandbox = attribute()
  def sandbox_=(value: String): Att[Iframe] = Attribute(value)
  
  implicit def seamless = attribute()
  def seamless_=(value: String): Att[Iframe] = Attribute(value)
  
  implicit def data = attribute()
  def data_=(value: String): Att[Object] = Attribute(value)
  
  implicit def poster = attribute()
  def poster_=(value: String): Att[Video] = Attribute(value)
  
  implicit def autobuffer = attribute()
  def autobuffer_=(value: String): Att[Video with Audio] = Attribute(value)
  
  implicit def autoplay = attribute()
  def autoplay_=(value: String): Att[Video with Audio] = Attribute(value)
  
  implicit def loop = attribute()
  def loop_=(value: String): Att[Video with Audio] = Attribute(value)
  
  implicit def controls = attribute()
  def controls_=(value: String): Att[Video with Audio] = Attribute(value)
  
  implicit def coords = attribute()
  def coords_=(value: String): Att[Area] = Attribute(value)
  
  implicit def shape = attribute()
  def shape_=(value: String): Att[Area] = Attribute(value)
  
  implicit def headers = attribute()
  def headers_=(value: Int): Att[Td with Th] = Attribute(value.toString)
  
  implicit def scope = attribute()
  def scope_=(value: String): Att[Th] = Attribute(value.toString)
  
  private type FormAttribute = Object with Fieldset with Label with Input with Button with Select with Textarea
      with Output
  implicit def form = attribute()
  def form_=(value: String): Att[FormAttribute] = Attribute(value.toString)
  
  implicit def acceptCharset = attribute()
  def acceptCharset_=(value: Encoding): Att[Form] = Attribute(value.name)
  
  implicit def autocomplete = attribute()
  def autocomplete_=(value: Boolean): Att[Form with Input] = Attribute(if(value) "on" else "off")
  
  implicit def novalidate = attribute()
  def novalidate_=(value: Boolean): Att[Form with Input with Button] = Attribute(if(value) "novalidate" else null)
  
  private type DisabledAttribute = Fieldset with Input with Button with Select with Optgroup with Option with
      Textarea with Command
  implicit def disabled = attribute()
  def disabled_=(value: Boolean): Att[DisabledAttribute] = Attribute(if(value) "disabled" else null)
  
  implicit def label = attribute()
  def label_=(value: String): Att[Option with Command with Menu] = Attribute(value)

  implicit def forName = attribute()
  def forName_=(value: String): Att[Option with Command with Menu] = Attribute(value)
  
  implicit def accept = attribute()
  def accept_=(value: String): Att[Input with Menu] = Attribute(value)

  type AutofocusAttribute = Input with Button with Select with Textarea
  implicit def autofocus = attribute()
  def autofocus_=(value: Boolean): Att[AutofocusAttribute] = Attribute(if(value) "autofocus" else null)
  
  implicit def list = attribute()
  def list_=(value: String): Att[Input] = Attribute(value)
  
  implicit def multiple = attribute()
  def multiple_=(value: Boolean): Att[Input with Select] = Attribute(if(value) "multiple" else null)
  
  implicit def pattern = attribute()
  def pattern_=(value: String): Att[Input] = Attribute(value)
  
  implicit def placeholder = attribute()
  def placeholder_=(value: String): Att[Input] = Attribute(value)
  
  implicit def readonly = attribute()
  def readonly_=(value: Boolean): Att[Input with Textarea] = Attribute(if(value) "readonly" else null)
  
  implicit def required = attribute()
  def required_=(value: Boolean): Att[Input with Textarea] = Attribute(if(value) "required" else null)
  
  implicit def size = attribute()
  def size_=(value: Int): Att[Input with Select] = Attribute(value.toString)
  
  implicit def step = attribute()
  def step_=(value: String): Att[Input] = Attribute(value)
  
  implicit def icon = attribute()
  def icon_=(value: String): Att[Command] = Attribute(value)
  
  implicit def radiogroup = attribute()
  def radiogroup_=(value: String): Att[Command] = Attribute(value)
  
  implicit def default = attribute()
  def default_=(value: String): Att[Command] = Attribute(value)
  */
  case class TypeOption(typeName: String) extends AnyVal {
    override implicit def toString = typeName
  }

  val hidden = TypeOption("hidden")
  val text = TypeOption("text")
  val button = TypeOption("button")
  val tel = TypeOption("tel")
  val url = TypeOption("url")
  val email = TypeOption("email")
  val password = TypeOption("password")
  val date = TypeOption("date")
  val month = TypeOption("month")
  val week = TypeOption("week")
  val datetimeLocal = TypeOption("datetime-local")
  val time = TypeOption("time")
  val number = TypeOption("number")
  val range = TypeOption("range")
  val color = TypeOption("color")
  val checkbox = TypeOption("checkbox")
  val radio = TypeOption("radio")
  val file = TypeOption("file")
  val submit = TypeOption("submit")
  val image = TypeOption("image")
  val reset = TypeOption("reset")

}

