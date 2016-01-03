# Rapture HTML

Rapture HTML provides simple but typesafe support for working with HTML5
documents in Scala, enforcing the constraints of the document object model.
It is a small library which works independently of any other web framework.

Rapture HTML is part of the [Rapture](http://rapture.io/) project.

## Features

 - Clean, elegant and intuitive DSL for creating HTML tree structures
 - Nesting constraints on HTML nodes are enforced by the type system
 - HTML attributes may only be applied to appropriate elements
 - Customized explanatory error messages given for badly-nested elements and
   mismatched attributes
 - Attributes generally use rich Scala types, not `String`s
 - HTML Document object model specification is concise, extensible and
   maintainable
 - Lightweight, typesafe accessors for navigating elements and attributes

## Availability

Rapture HTML 0.1.0 is available under the *Apache 2.0 License* from Maven Central
with group ID `com.propensive` and artifact ID `rapture-html_2.10`.

### SBT

You can include Rapture HTML as a dependency in your own project by adding the
following library dependency to your build file:

```scala
libraryDependencies ++= Seq("com.propensive" %% "rapture-html" % "0.1.0")
```

### Maven

If you use Maven, include the following dependency:

```xml
<dependency>
  <groupId>com.propensive</groupId>
  <artifactId>rapture-html_2.10</artifactId>
  <version>0.1.0</version>
</dependency>
```

### Building from source with SBT

To build Rapture HTML from source, follow these steps:

```
git clone git@github.com:propensive/rapture-html.git
cd rapture-html
sbt package
```

If the compilation is successful, the compiled JAR file should be found in the
directory for the appropriate Scala version in the `target` directory.

## Status

Rapture HTML is *experimental*. This means that the API is undergoing change,
and new features may be added or removed without changes being documented.

## Contributing

Rapture HTML -- like all the Rapture projects -- openly welcomes contributions!
We would love to receive pull requests of bugfixes and enhancements from other
developers. To avoid potential wasted effort, bugs should first be reported on
the Github issue tracker, and it's normally a good idea to talk about
enhancements on the Rapture mailing list before embarking on any development.
Alternatively, just send Jon Pretty (@propensive) a tweet to start a
conversation.

# Using Rapture HTML

First of all, import the following:

```
import rapture.html._
import htmlSyntax._
```

The `htmlSyntax` package comprises a large namespace containing all HTML tag
and attribute names, so it is recommended to import this only into scopes where
it is to be immediately used.

## HTML Elements

A simple HTML element, with no attributes or child elements is as simple as
writing the tag name, with its first letter capitalized, for example,

```
// <form/>
val html = Form
```

To add attributes to an empty tag, list them like named parameters to the element, like this,

```
// <form id="signup" method="POST"/>
val html = Form(id = 'signup, method = Post)
```

Note that the types of the attribute values are not, in general, `String`s.
Each attribute is typed according to the values it allows to be assigned.
A later version of Rapture HTML will offer multiple alternative types
(originating from third-party projects) for HTML attribute values.

Mostly, attribute names in Rapture HTML are identical to their native HTML5
counterparts, but in several cases it has been necessary to tweak the names.

 - Any attribute containing a `-` is converted to camel case, for example
   `http-equiv` becomes `httpEquiv`
 - The `class` attribute becomes `classes`
 - The `type` attrubute becomes `typeName`
 - The `for` attribute becomes `forName`
 - Custom `data-<name>` attributes become `data.<name>`

Better suggestions for more consistent handling of these attributes would be
very welcome.

An HTML element may also have child elements, by adding them as parameters in a
second parameter block, like so,

```
// <form id="signup" method="POST"><label/><input/></form>
val html = Form(id = 'signup, method = Post)(Label, Input)
```

Remember, `Label` and `Input` are just empty elements.

The first parameter block of any HTML element, however, is completely optional
and can be omitted if no attributes are required. For example,

```
// <form><label/><input/></form>
val html = Form(Label, Input)
```

## Cautionary notice

Scala does not offer any native support for an optional initial parameter
block, and the complex machinery necessary to offer this, whilst maintaining
idiomatic-looking syntax may be best described as "baroque". Until this
approach has been extensively tested, users should be aware that it may be an
imperfect abstraction.

This syntax would not have been possible without help from *all* of the
following Scala features, mostly as a workaround for overloading interacting
badly with type inference:

 - Implicit parameters and conversions
 - Getters and setters
 - Dependent method types
 - Higher-kinded types
 - Macros
 - Dynamic type
 - Singleton types
 - Inheritance and variance

Bug reports around this approach would be very welcome.

## Example HTML document

A slightly more complete HTML document may look like this:

```
val html = Html(
  Head(
    Title("Hello world!")
  ),
  Body(
    H1("Hello World!"),
    Ul(
      Li(id = 'first)("Apple"),
      Li("Orange"),
      Li("Pear")
    )
  )
)
```

This works because each HTML element has been correctly placed within another
element that allows it as a child.

## Nesting rules

Almost all of HTML5's rules on nesting elements, as specified in the working
draft, are enforced by Rapture HTML.

Whilst it is possible to write `Table(Tbody(Tr(Td)))`, a compile error would be
produced by `Html(Head(Table))`, because in HTML, a `<table>` element cannot
appear inside a `<head>` element.

The following custom error is produced at compile time:

```
error: Attempted to nest a Flow node in a position where only Metadata nodes are permitted
Html(Head(Table))
          ^
```

The terms `Flow` and `Metadata` correspond to the terms used to describe
element types in the HTML5 Working Draft.

## Attribute rules

Likewise, attributes may only be applied to those elements which support them,
for example the `max` attribute is not valid on `Textarea` elements, so the
following line will result in a compile error.

```
Textarea(max = 100.0)
```

Unfortunately, possibly due to a type inference bug in Scalac, this error
currently presents itself as less friendly type error.

## Navigating an HTML document

Given an `Html` node, `html`, containing a document, nodes within that document
can be easily accessed using the `\` operator and named tags, for example,

```
val rows = html \ Body \ Div \ Div \ Table \ Tbody \ Tr
val firstRow = rows(0)
```

and the attributes of an existing HTML node may be accessed by dereferencing it
with the attribute name, for example, `firstRow.id` or `firstRow.style`. If the
attribute is present, it will be returned as the original type it was set as,
i.e.  not necessarily a `String`.

The `\\` operator will select all descendants (not just immediate children)
matching the specified tag.

```
val rows = html \\ Tr
val firstRow = rows(0)
```

All of these access methods take advantage of Rapture's
[modes](https://github.com/propensive/rapture-core), to give a choice about how
missing elements or attributes should be handled.

## Outputting HTML

The `toString` method on HTML elements will generate pretty-printed strings
which neatly indent the HTML source into a readable form, and by default the
`format` method will do the same. As an alternative, HTML may be output in
"compact" form, without any additional whitespace, by importing an alternative
formatter from the `rapture.dom` package, for example,

```
import rapture.dom._
import domFormatters.compact._

println(myHtml.format)
```

Formatters are pluggable, so it is easy to write a custom formatter which
outputs the HTML in a different form, not necessarily to a `String`.

## The HTML Document Object Model

The rules governing how HTML elements may be nested are defined in the [syntax
file](https://github.com/propensive/rapture-html/blob/master/src/syntax.scala)
based on some very simple [phantom
types](https://github.com/propensive/rapture-html/blob/master/src/phantom.scala)
which determine the name, basic types, child types and attribute types of each
tag.

The current implementation is based on a version from an HTML5 working draft
from 2013. It is likely that the current implementation is incomplete or
incorrect in places, so corrections and additions to these files are very
welcome.

Attribute definitions follow after the Tag definitions in the syntax file, but
are currently more verbose. Hopefully it will be possible to improve the
tersity of these definitions in a later release.

## Tests

There is currently no test suite for Rapture HTML, though implementing a
selection of tests is a medium-term priority.

## Other DOMs

The Rapture DOM project, a dependency of Rapture HTML, is designed to support
other HTML-like languages, just by defining tags, attributes and their
associated phantom types. Rapture HTML should serve as a reasonable starting
point for anyone looking to implement an alternative DOM language.

Additionally, for DOM-like languages which do not serialize to an SGML-like
syntax, but do follow the same basic structure, custom formatters may be
written to produce the appropriate output.

