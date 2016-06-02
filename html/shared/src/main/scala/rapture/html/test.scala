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
    trait Tr extends AttributeType
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
    /*trait Global extends AttributeType
    Html with Base with Link with Meta with Style with Script with Body with Blockquote with Ol
        with Li with A with Q with Time with Progress with Meter with Bdo with Edit with Img with Iframe with Embed
        with Object with Param with Video with Audio with Source with Canvas with Map with Area with Col with Td with
        Th with Form with Fieldset with Label with Input with Button with Select with Optgroup with Option with
        Textarea with Output with Details with Command with Bb with Menu with Tr*/

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
    trait NoChildren extends NodeType
    trait NoParent extends NodeType
  }

  import Html5._

  // Missing: Title, A, Abbr, Ins, Del, Object, Video, Audio, Canvas
 
  import Node._

  object Attributes {
    trait Global extends AttributeType
    trait Span extends Global
    trait Td extends Global
    trait Th extends Td
    trait Table extends Global
  }

  val Html       = Empty[NoParent,                  NoAttributes, Top](Optional)
  val Head       = Empty[Top,                       NoAttributes, Metadata](Optional)
  val Base       = Empty[Metadata,                  NoAttributes, NoChildren](EmptyTag)
  val Title      = Empty[Metadata,                  NoAttributes, NoChildren]()
  val Link       = Empty[Metadata,                  NoAttributes, NoChildren](EmptyTag)
  val Meta       = Empty[Metadata,                  NoAttributes, Metadata](EmptyTag)
  val Style      = Empty[Metadata with Flow,        NoAttributes, Text]()
  val Script     = Empty[Metadata with Phrasing,    NoAttributes, Text]()
  val Noscript   = Empty[Metadata with Phrasing,    NoAttributes, Text]()
  val Body       = Empty[Top,                       NoAttributes, Flow](Optional)
  val Section    = Empty[Sectioning,                NoAttributes, Flow]()
  val Nav        = Empty[Sectioning,                NoAttributes, Flow]()
  val Article    = Empty[Sectioning,                NoAttributes, Flow]()
  val Aside      = Empty[Sectioning,                NoAttributes, Flow]()
  val H1         = Empty[Heading,                   NoAttributes, Phrasing]()
  val H2         = Empty[Heading,                   NoAttributes, Phrasing]()
  val H3         = Empty[Heading,                   NoAttributes, Phrasing]()
  val H4         = Empty[Heading,                   NoAttributes, Phrasing]()
  val H5         = Empty[Heading,                   NoAttributes, Phrasing]()
  val H6         = Empty[Heading,                   NoAttributes, Phrasing]()
  val Header     = Empty[Heading,                   NoAttributes, Flow]()
  val Footer     = Empty[Flow,                      NoAttributes, Flow]()
  val Address    = Empty[Flow,                      NoAttributes, Flow]()
  val P          = Empty[Flow,                      NoAttributes, Phrasing](ClosingTagOptional)
  val Hr         = Empty[Flow,                      NoAttributes, NoChildren](EmptyTag)
  val Br         = Empty[NodeType,                  NoAttributes, NoChildren](EmptyTag)
  val Pre        = Empty[Flow,                      NoAttributes, Phrasing]()
  val Dialog     = Empty[Flow,                      NoAttributes, Definitions]()
  val Blockquote = Empty[Sectioning,                NoAttributes, Flow]()
  val Ol         = Empty[Flow,                      NoAttributes, ListItems]()
  val Ul         = Empty[Flow,                      NoAttributes, ListItems]()
  val Li         = Empty[ListItems,                 NoAttributes, Flow](ClosingTagOptional)
  val Dl         = Empty[Flow,                      NoAttributes, Definitions]()
  val Dt         = Empty[Definitions,               NoAttributes, Phrasing](ClosingTagOptional)
  val Dd         = Empty[Definitions,               NoAttributes, Flow](ClosingTagOptional)
  // val A = Empty[_, _, _]()
  val Q          = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Cite       = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Em         = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Strong     = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Small      = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Mark       = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Dfn        = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Time       = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Progress   = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Meter      = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Code       = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Var        = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Samp       = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Kbd        = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Sup        = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Sub        = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Span       = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val I          = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val B          = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Bdo        = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Ruby       = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Rt         = Empty[Phrasing,                  NoAttributes, Nothing]()
  val Rp         = Empty[Phrasing,                  NoAttributes, Nothing]()
  val Figure     = Empty[Sectioning,                NoAttributes, Flow]()
  val Img        = Empty[Embedded,                  NoAttributes, Nothing](EmptyTag)
  val Iframe     = Empty[Embedded,                  NoAttributes, Text]()
  val Embed      = Empty[Embedded,                  NoAttributes, Text](EmptyTag)
  //val Object     = Empty[Embedded, Embed, Text]()
  val Param      = Empty[Embedded,                  NoAttributes, Flow](EmptyTag)
  // val Video      = Empty[]()
  // val Audio      = Empty[]()
  val Source     = Empty[Flow,                      NoAttributes, Nothing](EmptyTag)
  // Canvas
  val Map        = Empty[Flow,                      NoAttributes, Flow]()
  val Area       = Empty[Phrasing,                  NoAttributes, Nothing](EmptyTag)
  val Table      = Empty[Flow,                      Attributes.Global with Attributes.Table, TableItems]()
  val Caption    = Empty[TableItems,                NoAttributes, Phrasing]()
  val Colgroup   = Empty[TableItems,                Attributes.Global with Attributes.Span, ColItems](Optional)
  val Col        = Empty[ColItems,                  Attributes.Global with Attributes.Span, Nothing](EmptyTag)
  val Tbody      = Empty[TableItems,                Attributes.Global, TrItems](Optional)
  val Thead      = Empty[TableItems,                Attributes.Global, TrItems](Optional)
  val Tfoot      = Empty[TableItems,                Attributes.Global, TrItems](Optional)
  val Tr         = Empty[TrItems,                   Attributes.Global, TdItems](ClosingTagOptional)
  val Td         = Empty[TdItems,                   Attributes.Td, Flow](ClosingTagOptional)
  val Th         = Empty[TdItems,                   Attributes.Th, Flow](ClosingTagOptional)
  val Form       = Empty[Flow,                      NoAttributes, Flow]()
  val Fieldset   = Empty[Flow,                      NoAttributes, Flow]()
  val Label      = Empty[Phrasing with Interactive, NoAttributes, Phrasing]()
  val Input      = Empty[Phrasing with Interactive, NoAttributes, Nothing](EmptyTag)
  val Button     = Empty[Phrasing with Interactive, NoAttributes, Nothing]()
  val Select     = Empty[Phrasing with Interactive, NoAttributes, OptionItems]()
  val Datalist   = Empty[Phrasing with Interactive, NoAttributes, OptionItems]()
  val Optgroup   = Empty[Phrasing,                  NoAttributes, OptionItems](ClosingTagOptional)
  val Option     = Empty[OptionItems,               NoAttributes, Text](ClosingTagOptional)
  val Textarea   = Empty[Phrasing with Interactive, NoAttributes, Text]()
  val Output     = Empty[Phrasing,                  NoAttributes, Phrasing]()
  val Details    = Empty[Interactive,               NoAttributes, Flow]()
  val Command    = Empty[Metadata with Phrasing,    NoAttributes, Nothing](EmptyTag)
  val Bb         = Empty[Phrasing with Interactive, NoAttributes, Nothing]()
  val Menu       = Empty[Phrasing with Interactive, NoAttributes, ListItems]()
  val Legend     = Empty[Flow,                      NoAttributes, Phrasing]()
  val Div        = Empty[Flow,                      NoAttributes, Flow]()


  //def style = Attribute[Global, Css]("style")(_.content)
  //def style_=[That <: NodeType, ThatAtts <: AttributeType, This <: NodeType, Child <: NodeType, Atts2 <: AttributeType](v: Css)(implicit ev: ValidAttribute[ThatAtts, Global]): AttributeContent[That, ThatAtts, This, Child, Atts2] =
   // style.set[That, ThatAtts, This, Child, Atts2](v)

  implicit val accesskeyAttribute = EmbeddableAttribute[String, Attributes.Global]("accesskey")
  implicit val classAttribute = EmbeddableAttribute[String, Attributes.Global]("class")
  implicit val contenteditableAttribute = EmbeddableAttribute[String, Attributes.Global]("contenteditable")
  implicit val dirAttribute = EmbeddableAttribute[String, Attributes.Global]("dir")
  implicit val hiddenAttribute = EmbeddableAttribute[String, Attributes.Global]("hidden")
  implicit val idAttribute = EmbeddableAttribute[String, Attributes.Global]("id")
  implicit val langAttribute = EmbeddableAttribute[String, Attributes.Global]("lang")
  implicit val spellcheckAttribute = EmbeddableAttribute[String, Attributes.Global]("spellcheck")
  implicit val styleAttribute = EmbeddableAttribute[Css, Attributes.Global]("style")
  implicit val tabindexAttribute = EmbeddableAttribute[String, Attributes.Global]("tabindex")
  implicit val titleAttribute = EmbeddableAttribute[String, Attributes.Global]("title")
  implicit val translateAttribute = EmbeddableAttribute[String, Attributes.Global]("translate")
  
  implicit val borderAttribute = EmbeddableAttribute[String, Attributes.Table]("border")
  implicit val sortableAttribute = EmbeddableAttribute[Boolean, Attributes.Table]("sortable")
  
  implicit val spanAttribute = EmbeddableAttribute[String, Attributes.Span]("span")
  
  implicit val colspanAttribute = EmbeddableAttribute[Int, Attributes.Td]("colspan")
  implicit val rowspanAttribute = EmbeddableAttribute[Int, Attributes.Td]("rowspan")
  implicit val headersAttribute = EmbeddableAttribute[Symbol, Attributes.Td]("headers")
  
  implicit val scopeAttribute = EmbeddableAttribute[String, Attributes.Th]("scope")
  implicit val abbrAttribute = EmbeddableAttribute[String, Attributes.Th]("abbr")
  implicit val sortedAttribute = EmbeddableAttribute[String, Attributes.Th]("sorted")

}

object Testing {
  import HtmlTest._
  //val test = Html(style = css"color: bar")(Head, Body(style = css"color: bar")("Content"))
  //val test = Html(Head, Body(Table(Tbody(Tr(style = css"color:bar")(Td)))))

  //val test = Tr(style = css"display:none")
  val test3 = Tr.applyDynamicNamed("")(convert("style" -> css"display:none")(styleAttribute))(Td)
}
