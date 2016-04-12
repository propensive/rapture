package rapture.html2

import rapture.dom2._
import rapture.css._

object HtmlTest {
  object Html5 {
    trait NoAttributes extends AttributeType
    trait Html extends AttributeType
    trait Base extends AttributeType
    trait Link extends AttributeType
    trait Meta extends AttributeType
    trait Style extends AttributeType
    trait Script extends AttributeType
    trait Body extends AttributeType
    trait Blockquote extends AttributeType
    trait Ol extends AttributeType
    trait Li extends AttributeType
    trait A extends AttributeType
    trait Q extends AttributeType
    trait Time extends AttributeType
    trait Progress extends AttributeType
    trait Meter extends AttributeType
    trait Bdo extends AttributeType
    trait Edit extends AttributeType
    trait Img extends AttributeType
    trait Iframe extends AttributeType
    trait Embed extends AttributeType
    trait Object extends AttributeType
    trait Param extends AttributeType
    trait Video extends AttributeType
    trait Audio extends AttributeType
    trait Source extends AttributeType
    trait Canvas extends AttributeType
    trait Map extends AttributeType
    trait Area extends AttributeType
    trait Col extends AttributeType
    trait Td extends AttributeType
    trait Th extends AttributeType
    trait Form extends AttributeType
    trait Fieldset extends AttributeType
    trait Label extends AttributeType
    trait Input extends AttributeType
    trait Button extends AttributeType
    trait Select extends AttributeType
    trait Optgroup extends AttributeType
    trait Option extends AttributeType
    trait Textarea extends AttributeType
    trait Output extends AttributeType
    trait Details extends AttributeType
    trait Command extends AttributeType
    trait Bb extends AttributeType
    trait Menu extends AttributeType
    trait Global extends Html with Base with Link with Meta with Style with Script with Body with Blockquote with Ol
        with Li with A with Q with Time with Progress with Meter with Bdo with Edit with Img with Iframe with Embed
        with Object with Param with Video with Audio with Source with Canvas with Map with Area with Col with Td with
        Th with Form with Fieldset with Label with Input with Button with Select with Optgroup with Option with
        Textarea with Output with Details with Command with Bb with Menu

    trait Flow extends NodeType
    trait Metadata extends NodeType
    trait Top extends NodeType
    trait Definitions extends NodeType
    trait ListItems extends NodeType
    trait TableItems extends NodeType
    trait ColItems extends NodeType
    trait TrItems  extends NodeType
    trait TdItems extends NodeType
    trait OptionItems extends NodeType
    trait Sectioning extends Flow
    trait Heading extends Flow
    trait Interactive extends Flow
    trait Phrasing extends Flow
    trait Embedded extends Phrasing
    trait Text extends Phrasing
  }

  import Html5._

  val Html = Node.Empty[NodeType, Html, Top]()
  val Head = Node.Empty[Top, NoAttributes, Metadata]()
  val Base = Node.Empty[Metadata, Base, NodeType]()
  val Link = Node.Empty[Metadata, Link, NodeType]()
  val Meta = Node.Empty[Metadata, Meta, Metadata]()
  val Style = Node.Empty[Metadata with Flow, AttributeType, Text]()
  val Script = Node.Empty[Metadata with Phrasing, Script, Text]()
  val Noscript = Node.Empty[Metadata with Phrasing, AttributeType, Text]()
  val Body = Node.Empty[Top, Body, Flow]()
  val Section = Node.Empty[Sectioning, AttributeType, Flow]()
  val Nav = Node.Empty[Sectioning, AttributeType, Flow]()
  val Article = Node.Empty[Sectioning, AttributeType, Flow]()
  val Aside = Node.Empty[Sectioning, AttributeType, Flow]()
  val H1 = Node.Empty[Heading, AttributeType, Phrasing]()
  val H2 = Node.Empty[Heading, AttributeType, Phrasing]()
  val H3 = Node.Empty[Heading, AttributeType, Phrasing]()
  val H4 = Node.Empty[Heading, AttributeType, Phrasing]()
  val H5 = Node.Empty[Heading, AttributeType, Phrasing]()
  val H6 = Node.Empty[Heading, AttributeType, Phrasing]()
  val Header = Node.Empty[Heading, AttributeType, Flow]()
  val Footer = Node.Empty[Flow, AttributeType, Flow]()
  val Address = Node.Empty[Flow, AttributeType, Flow]()
  val P = Node.Empty[Flow, AttributeType, Phrasing]()
  val Hr = Node.Empty[Flow, AttributeType, NodeType]()
  val Br = Node.Empty[NodeType, AttributeType, Phrasing]()

  def style = Attribute[Global, Css]("style")(_.content)
  def style_=[That <: NodeType, ThatAtts <: AttributeType, This <: NodeType, Child <: NodeType, Atts2 <: AttributeType](v: Css)(implicit ev: Global <:< ThatAtts) =
    style.set[That, ThatAtts, This, Child, Atts2](v)

  

  val Pre = Node.Empty[Flow, AttributeType, Phrasing]()
  val Dialog = Node.Empty[Flow, AttributeType, Definitions]()
  val Blockquote = Node.Empty[Sectioning, Blockquote, Flow]()
  val Ol = Node.Empty[Flow, Ol, ListItems]()
  val Ul = Node.Empty[Flow, AttributeType, ListItems]()
  val Li = Node.Empty[ListItems, Li, Flow]()
  val Dl = Node.Empty[Flow, AttributeType, Definitions]()
  val Dt = Node.Empty[Definitions, AttributeType, Phrasing]()
  val Dd = Node.Empty[Definitions, AttributeType, Flow]()
  val Q = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Cite = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Em = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Strong = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Small = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Mark = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Dfn = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Time = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Progress = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Meter = Node.Empty[Phrasing, Meter, Phrasing]()
  val Code = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Var = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Samp = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Kbd = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Sup = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Sub = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Span = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val I = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val B = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Bdo = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Ruby = Node.Empty[Phrasing, AttributeType, Phrasing]()
  val Rt = Node.Empty[Phrasing, AttributeType, Nothing]()
  val Rp = Node.Empty[Phrasing, AttributeType, Nothing]()
  val Figure = Node.Empty[Sectioning, AttributeType, Flow]()
  val Img = Node.Empty[Embedded, Img, Nothing]()
  val Iframe = Node.Empty[Embedded, Iframe, Text]()
  val Embed = Node.Empty[Embedded, Embed, Text]()
  val Param = Node.Empty[Embedded, Param, Flow]()
  val Source = Node.Empty[Flow, Source, Nothing]()
  val Map = Node.Empty[Flow, Map, Flow]()
  val Area = Node.Empty[Phrasing, Area, Nothing]()
  val Table = Node.Empty[Flow, Area, TableItems]()
  val Caption = Node.Empty[TableItems, AttributeType, Phrasing]()
  val Colgroup = Node.Empty[TableItems, Col, ColItems]()
  val Col = Node.Empty[ColItems, Col, Nothing]()
  val Tbody = Node.Empty[TableItems, AttributeType, TrItems]()
  val Thead = Node.Empty[TableItems, AttributeType, TrItems]()
  val Tfoot = Node.Empty[TableItems, AttributeType, TrItems]()
  val Tr = Node.Empty[TrItems, AttributeType, TdItems]()
  val Td = Node.Empty[TdItems, Td, Flow]()
  val Th = Node.Empty[TdItems, Th, Flow]()
  val Form = Node.Empty[Flow, Form, Flow]()
  val Fieldset = Node.Empty[Flow, Fieldset, Flow]()
  val Label = Node.Empty[Phrasing with Interactive, Label, Phrasing]()
  val Input = Node.Empty[Phrasing with Interactive, Input, Nothing]()
  val Button = Node.Empty[Phrasing with Interactive, Button, Nothing]()
  val Select = Node.Empty[Phrasing with Interactive, Select, OptionItems]()
  val Datalist = Node.Empty[Phrasing with Interactive, AttributeType, OptionItems]()
  val Optgroup = Node.Empty[Phrasing, Optgroup, OptionItems]()
  val Option = Node.Empty[OptionItems, Option, Text]()
  val Textarea = Node.Empty[Phrasing with Interactive, Textarea, Text]()
  val Output = Node.Empty[Phrasing, Output, Phrasing]()
  val Details = Node.Empty[Interactive, Details, Flow]()
  val Command = Node.Empty[Metadata with Phrasing, Command, Nothing]()
  val Bb = Node.Empty[Phrasing with Interactive, Bb, Nothing]()
  val Menu = Node.Empty[Phrasing with Interactive, Menu, ListItems]()
  val Legend = Node.Empty[Flow, AttributeType, Phrasing]()
  val Div = Node.Empty[Flow, AttributeType, Flow]()

}

object Testing {
  import HtmlTest._
  //val test = Html(style = css"foo: bar")(Head, Body(style = css"foo: bar")("Content"))
  val test = Table(style = css"foo:bar")(Tbody(style = css"foo:bar")(Tr(style = css"foo:bar")(Td)))
}
