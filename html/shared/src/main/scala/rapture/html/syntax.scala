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

package rapture.html

import rapture.dom._
import rapture.core._
import rapture.codec._
import rapture.net._
import rapture.uri._
import rapture.js._
import rapture.css._

import language.{dynamics, implicitConversions}

trait DynamicCssReferencing

object dynamicCssReferencing {
  def apply(): DynamicCssReferencing = dynamicCssReferencingImplicit
  implicit val dynamicCssReferencingImplicit: DynamicCssReferencing = new DynamicCssReferencing {}
}

object htmlSyntax {

  import Html5._

  class DynamicAttributeKey[Name <: String, Att <: AttributeType, Val](name: String, ser: Val => String) extends AttributeKey[Name, Att](name) with Dynamic {
    type Value = Val
    def serialize(v: Value): String = ser(v)

    def selectDynamic(att: String) = Attribute[Global, String](s"$name-$att")(identity(_))
    def updateDynamic[E <: ElementType](att: String)(v: String) = selectDynamic(att).set[E](v)
  }

  def dynamic[Att <: AttributeType, Val](name: String, actualName: String = null)(serializer: Val => String) = new DynamicAttributeKey[name.type, Att, Val](if(actualName == null) name else actualName, serializer)


  implicit def stringToTextNode(str: String): TextNode[Nothing, Nothing, Html5.Text] =
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
  val Style = Tag[Text, Metadata with Flow, AttributeType](block = true)
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
  val Pre = Tag[Phrasing, Flow, AttributeType](block = false)
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
  val Em = Tag[Phrasing, Phrasing, AttributeType](block = false)
  val Strong = Tag[Phrasing, Phrasing, AttributeType](block = false)
  val Small = Tag[Phrasing, Phrasing, AttributeType]()
  val Mark = Tag[Phrasing, Phrasing, AttributeType]()
  val Dfn = Tag[Phrasing, Phrasing, AttributeType]()
  val Time = Tag[Phrasing, Phrasing, AttributeType]()
  val Progress = Tag[Phrasing, Phrasing, AttributeType]()
  val Meter = Tag[Phrasing, Phrasing, Meter]()
  val Code = Tag[Phrasing, Phrasing, AttributeType](block = false)
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
  val Legend = Tag[Phrasing, Flow, AttributeType]()
  val Div = Tag[Flow, Flow, AttributeType](forceClosingTag = true)

  implicit def id = Attribute[Global, String]("id")(identity)
  def id_=[E <: ElementType, Value: DomIdable](v: Value) = id.set[E](implicitly[DomIdable[Value]].domId(v))

  implicit def lang = Attribute[Global, Symbol]("lang")(_.name)
  def lang_=[E <: ElementType](v: Symbol) = lang.set[E](v)

  implicit def translate = Attribute[Global, Boolean]("translate")(v => if (v) "yes" else "no")
  def translate_=[E <: ElementType](v: Boolean) = translate.set[E](v)

  object CssClassable {
    
    implicit val cssClassable: CssClassable[CssClass] =
      new CssClassable[CssClass] { def cssClass(cssClass: CssClass) = cssClass.classes.to[List] }
    
    implicit def stringCssClassable(implicit dynamicCssReferencing: DynamicCssReferencing): CssClassable[String] =
      new CssClassable[String] { def cssClass(string: String) = List(string) }
    
    implicit def symbolCssClassable(implicit dynamicCssReferencing: DynamicCssReferencing): CssClassable[Symbol] =
      new CssClassable[Symbol] { def cssClass(symbol: Symbol) = List(symbol.name) }
    
    implicit def stringListCssClassable(implicit dynamicCssReferencing: DynamicCssReferencing): CssClassable[List[String]] =
      new CssClassable[List[String]] { def cssClass(list: List[String]) = list }
    
    implicit def symbolListCssClassable(implicit dynamicCssReferencing: DynamicCssReferencing): CssClassable[List[Symbol]] =
      new CssClassable[List[Symbol]] { def cssClass(list: List[Symbol]) = list.map(_.name) }
  }

  trait CssClassable[Value] { def cssClass(value: Value): List[String] }

  object DomIdable {
    
    implicit val domIdable: DomIdable[DomId] =
      new DomIdable[DomId] { def domId(value: DomId): String = value.id }
    
    implicit def stringDomIdable(implicit dynamicCssReferencing: DynamicCssReferencing): DomIdable[String] =
      new DomIdable[String] { def domId(string: String): String = string }
    
    implicit def symbolDomIdable(implicit dynamicCssReferencing: DynamicCssReferencing): DomIdable[Symbol] =
      new DomIdable[Symbol] { def domId(symbol: Symbol): String = symbol.name }
  }

  trait DomIdable[Value] { def domId(value: Value): String }

  implicit def cls = Attribute[Global, Seq[String]]("cls", "class")(_.mkString(" "))
  def cls_=[E <: ElementType, Value: CssClassable](value: Value) =
    cls.set(implicitly[CssClassable[Value]].cssClass(value))

  implicit def onload = Attribute[Body, Js]("onload")(_.content)
  def onload_=[E <: ElementType](v: Js) = onload.set[E](v)

  implicit def onclick = Attribute[Global, Js]("onclick")(_.content)
  def onclick_=[E <: ElementType](v: Js) = onclick.set[E](v)

  implicit def onmouseover = Attribute[Global, Js]("onmouseover")(_.content)
  def onmouseover_=[E <: ElementType](v: Js) = onmouseover.set(v)

  implicit def onmouseout = Attribute[Global, Js]("onmouseout")(_.content)
  def onmouseout_=[E <: ElementType](v: Js) = onmouseout.set(v)

  implicit def onchange = Attribute[Input, Js]("onchange")(_.content)
  def onchange_=[E <: ElementType](v: Js) = onchange.set(v)

  implicit def onblur = Attribute[Input, Js]("onblur")(_.content)
  def onblur_=[E <: ElementType](v: Js) = onblur.set(v)

  implicit def title = Attribute[Global, String]("title")(identity)
  def title_=[E <: ElementType](v: String) = title.set[E](v)

  implicit def alt = Attribute[Img with Area with Input, String]("alt")(identity)
  def alt_=[E <: ElementType](v: String) = alt.set[E](v)

  implicit def href = Attribute[Base with Link with A with Area, PathLink]("href")(_.link)
  def href_=[E <: ElementType, L: Linkable](v: L) = href.set[E](implicitly[Linkable[L]].link(v))

  implicit def name =
    Attribute[
        Meta with Iframe with Object with Param with Map with Form with Fieldset with Input with Button with Select with Textarea with Output,
        Symbol]("name")(_.name)
  def name_=[E <: ElementType](v: Symbol) = name.set[E](v)

  implicit def selected = Attribute[Option, Boolean]("selected") { v =>
    if (v) "selected" else null
  }
  def selected_=[E <: ElementType](v: Boolean) = selected.set[E](v)

  implicit def cols = Attribute[Textarea, Int]("cols")(_.toString)
  def cols_=[E <: ElementType](v: Int) = cols.set[E](v)

  implicit def rows = Attribute[Textarea, Int]("rows")(_.toString)
  def rows_=[E <: ElementType](v: Int) = rows.set[E](v)

  implicit def colspan = Attribute[Td with Th, Int]("colspan")(_.toString)
  def colspan_=[E <: ElementType](v: Int) = colspan.set[E](v)

  implicit def rowspan = Attribute[Td with Th, Int]("rowspan")(_.toString)
  def rowspan_=[E <: ElementType](v: Int) = rowspan.set[E](v)

  implicit def wrap = Attribute[Textarea, Boolean]("wrap") { v =>
    if (v) "wrap" else null
  }
  def wrap_=[E <: ElementType](v: Boolean) = wrap.set[E](v)

  implicit def open = Attribute[Details, Boolean]("open") { v =>
    if (v) "open" else null
  }
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

  implicit def style = Attribute[Global, Css]("style")(_.content)
  def style_=[E <: ElementType](v: Css) = style.set[E](v)

  implicit def src =
    Attribute[Script with Img with Iframe with Embed with Video with Audio with Source with Input, PathLink]("src")(
        _.toString)
  def src_=[E <: ElementType, L: Linkable](v: L) = src.set[E](implicitly[Linkable[L]].link(v))

  implicit def value =
    Attribute[Li with Progress with Meter with Param with Input with Button with Option, String]("value")(identity)
  def value_=[E <: ElementType](v: String) = value.set[E](v)

  implicit def typ =
    Attribute[
        Link with Style with Script with A with Embed with Object with Source with Area with Input with Button with Command with Bb with Menu,
        String]("typ", "type")(identity)
  def typ_=[E <: ElementType](v: String) = typ.set[E](v)

  implicit def action = Attribute[Form with Input with Button, PathLink]("action")(_.link)
  def action_=[E <: ElementType, L: Linkable](v: L) = action.set(implicitly[Linkable[L]].link(v))

  implicit def method = Attribute[Form with Input with Button, String]("method")(identity)
  def method_=[E <: ElementType](v: String) = method.set(v)

  implicit def enctype = Attribute[Form with Input with Button, String]("enctype")(identity)
  def enctype_=[E <: ElementType](v: String) = enctype.set(v)

  implicit def checked = Attribute[Input, Boolean]("checked")(v => if (v) "checked" else null)
  def checked_=[E <: ElementType](v: Boolean) = checked.set(v)

  implicit def maxlength = Attribute[Input with Textarea, Int]("maxlength")(_.toString)
  def maxlength_=[E <: ElementType](v: Int) = maxlength.set(v)

  implicit def hreflang = Attribute[Link, Symbol]("hreflang")(_.name)
  def hreflang_=[E <: ElementType](v: Symbol) = hreflang.set(v)

  implicit def sizes = Attribute[Link, String]("sizes")(identity)
  def sizes_=[E <: ElementType](v: String) = sizes.set(v)

  implicit def scoped = Attribute[Style, Boolean]("scoped")(v => if (v) "scoped" else null)
  def scoped_=[E <: ElementType](v: Boolean) = scoped.set(v)

  implicit def async = Attribute[Script, Boolean]("async")(v => if (v) "async" else null)
  def async_=[E <: ElementType](v: Boolean) = async.set(v)

  implicit def defer = Attribute[Script, Boolean]("defer")(v => if (v) "defer" else null)
  def defer_=[E <: ElementType](v: Boolean) = defer.set(v)

  implicit def onbeforeunload = Attribute[Body, Js]("onbeforeunload")(_.content)
  def onbeforeunload_=[E <: ElementType](v: Js) = onbeforeunload.set(v)

  implicit def onerror = Attribute[Body, Js]("onerror")(_.content)
  def onerror_=[E <: ElementType](v: Js) = onerror.set(v)

  implicit def onhashchange = Attribute[Body, Js]("onhashchange")(_.content)
  def onhashchange_=[E <: ElementType](v: Js) = onhashchange.set(v)

  implicit def onmessage = Attribute[Body, Js]("onmessage")(_.content)
  def onmessage_=[E <: ElementType](v: Js) = onmessage.set(v)

  implicit def onoffline = Attribute[Body, Js]("onoffline")(_.content)
  def onoffline_=[E <: ElementType](v: Js) = onoffline.set(v)

  implicit def onpopstate = Attribute[Body, Js]("onpopstate")(_.content)
  def onpopstate_=[E <: ElementType](v: Js) = onpopstate.set(v)

  implicit def onresize = Attribute[Body, Js]("onresize")(_.content)
  def onresize_=[E <: ElementType](v: Js) = onresize.set(v)

  implicit def onstorage = Attribute[Body, Js]("onstorage")(_.content)
  def onstorage_=[E <: ElementType](v: Js) = onstorage.set(v)

  implicit def onunload = Attribute[Body, Js]("onunload")(_.content)
  def onunload_=[E <: ElementType](v: Js) = onunload.set(v)

  implicit def reversed = Attribute[Ol, Boolean]("reversed")(v => if (v) "reversed" else null)
  def reversed_=[E <: ElementType](v: Boolean) = reversed.set(v)

  implicit def start = Attribute[Ol, Int]("start")(_.toString)
  def start_=[E <: ElementType](v: Int) = start.set(v)

  implicit def ping = Attribute[Link with Area, PathLink]("ping")(_.link)
  def ping_=[E <: ElementType, L: Linkable](v: L) = ping.set(implicitly[Linkable[L]].link(v))

  implicit def cite = Attribute[Blockquote with Q with Edit, PathLink]("cite")(_.link)
  def cite_=[E <: ElementType, L: Linkable](v: L) = cite.set(implicitly[Linkable[L]].link(v))

  implicit def datetime = Attribute[Time with Edit, String]("datetime")(identity)
  def datetime_=[E <: ElementType](v: String) = datetime.set(v)

  implicit def dir = Attribute[Global, Symbol]("dir")(_.name)
  def dir_=[E <: ElementType](v: Symbol) = dir.set(v)

  implicit def usemap = Attribute[Img with Object, String]("usemap")(identity)
  def usemap_=[E <: ElementType](v: String) = usemap.set(v)

  implicit def ismap = Attribute[Img, Boolean]("ismap")(v => if (v) "ismap" else null)
  def ismap_=[E <: ElementType](v: Boolean) = ismap.set(v)

  implicit def width =
    Attribute[Img with Iframe with Embed with Object with Video with Canvas with Input, Int]("width")(_.toString)
  def width_=[E <: ElementType](v: Int) = width.set(v)

  implicit def height =
    Attribute[Img with Iframe with Embed with Object with Video with Canvas with Input, Int]("height")(_.toString)
  def height_=[E <: ElementType](v: Int) = height.set(v)

  implicit def sandbox = Attribute[Iframe, Boolean]("sandbox")(v => if (v) "sandbox" else null)
  def sandbox_=[E <: ElementType](v: Boolean) = sandbox.set(v)

  implicit def seamless = Attribute[Iframe, Boolean]("seamless")(v => if (v) "seamless" else null)
  def seamless_=[E <: ElementType](v: Boolean) = seamless.set(v)

  implicit def poster = Attribute[Video, PathLink]("poster")(_.link)
  def poster_=[E <: ElementType, L: Linkable](v: L) = poster.set(implicitly[Linkable[L]].link(v))

  implicit def data = dynamic[Object, String]("data")(identity)
  def data_=[E <: ElementType](v: String) = data.set(v)

  implicit def autobuffer = Attribute[Video with Audio, Boolean]("autobuffer")(v => if (v) "autobuffer" else null)
  def autobuffer_=[E <: ElementType](v: Boolean) = autobuffer.set(v)

  implicit def autoplay = Attribute[Video with Audio, Boolean]("autoplay")(v => if (v) "autoplay" else null)
  def autoplay_=[E <: ElementType](v: Boolean) = autoplay.set(v)

  implicit def loop = Attribute[Video with Audio, Boolean]("loop")(v => if (v) "loop" else null)
  def loop_=[E <: ElementType](v: Boolean) = loop.set(v)

  implicit def controls = Attribute[Video with Audio, Boolean]("controls")(v => if (v) "controls" else null)
  def controls_=[E <: ElementType](v: Boolean) = controls.set(v)

  implicit def coords = Attribute[Area, String]("coords")(identity)
  def coords_=[E <: ElementType](v: String) = coords.set(v)

  implicit def shape = Attribute[Area, String]("shape")(identity)
  def shape_=[E <: ElementType](v: String) = shape.set(v)

  implicit def headers = Attribute[Td with Th, Symbol]("headers")(_.name)
  def headers_=[E <: ElementType](v: Symbol) = headers.set(v)

  implicit def scope = Attribute[Th, Symbol]("scope")(_.name)
  def scope_=[E <: ElementType](v: Symbol) = scope.set(v)

  implicit def acceptCharset = Attribute[Form, String]("accept-charset")(identity)
  def acceptCharset_=[E <: ElementType](v: String) = acceptCharset.set(v)

  implicit def autocomplete = Attribute[Form with Input, Boolean]("autocomplete")(v => if (v) "on" else "off")
  def autocomplete_=[E <: ElementType](v: Boolean) = autocomplete.set(v)

  implicit def novalidate =
    Attribute[Form with Input with Button, Boolean]("novalidate")(v => if (v) "novalidate" else null)
  def novalidate_=[E <: ElementType](v: Boolean) = novalidate.set(v)

  implicit def label = Attribute[Option with Command with Menu, String]("label")(identity)
  def label_=[E <: ElementType](v: String) = label.set(v)

  implicit def forName = Attribute[Option with Command with Menu, Symbol]("forName")(_.name)
  def forName_=[E <: ElementType](v: Symbol) = forName.set(v)

  implicit def `for` = Attribute[Label, Symbol]("for")(_.name)
  def for_=[E <: ElementType](v: Symbol) = `for`.set(v)

  implicit def accept = Attribute[Input with Menu, String]("accept")(identity)
  def accept_=[E <: ElementType](v: String) = accept.set(v)

  implicit def autofocus =
    Attribute[Input with Button with Select with Textarea, Boolean]("autofocus")(v => if (v) "autofocus" else null)
  def autofocus_=[E <: ElementType](v: Boolean) = autofocus.set(v)

  implicit def list = Attribute[Input, Symbol]("list")(_.name)
  def list_=[E <: ElementType](v: Symbol) = list.set(v)

  implicit def multiple = Attribute[Input with Select, Boolean]("multiple")(v => if (v) "multiple" else null)
  def multiple_=[E <: ElementType](v: Boolean) = multiple.set(v)

  implicit def pattern = Attribute[Input, String]("pattern")(identity)
  def pattern_=[E <: ElementType](v: String) = pattern.set(v)

  implicit def placeholder = Attribute[Input, String]("placeholder")(identity)
  def placeholder_=[E <: ElementType](v: String) = placeholder.set(v)

  implicit def readonly = Attribute[Input with Textarea, Boolean]("readonly")(v => if (v) "readonly" else null)
  def readonly_=[E <: ElementType](v: Boolean) = readonly.set(v)

  implicit def required = Attribute[Input with Textarea, Boolean]("required")(v => if (v) "required" else null)
  def required_=[E <: ElementType](v: Boolean) = required.set(v)

  implicit def size = Attribute[Input with Select, Int]("size")(_.toString)
  def size_=[E <: ElementType](v: Int) = size.set(v)

  implicit def step = Attribute[Input, Int]("step")(_.toString)
  def step_=[E <: ElementType](v: Int) = step.set(v)

  implicit def icon = Attribute[Command, PathLink]("icon")(_.link)
  def icon_=[E <: ElementType, L: Linkable](v: L) = icon.set(implicitly[Linkable[L]].link(v))

  implicit def radiogroup = Attribute[Command, Symbol]("radiogroup")(_.name)
  def radiogroup_=[E <: ElementType](v: Symbol) = radiogroup.set(v)

  implicit def default = Attribute[Command, String]("default")(identity)
  def default_=[E <: ElementType](v: String) = default.set(v)

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
