
# Rapture 2, Milestone 4: "Toltec"
## 3 February, 2016

This is the first time we're producing Release Notes for Rapture, but we will
aim to do it for all future milestones. So, here is a summary of the main
changes since Rapture 2.0.0-M3, organised by module.

## IO

### Support for unzipping streams

A new capability has been included to allow streams to be unzipped, regardless
of their origin (file, URL, classpath, etc), providing an iterator over
`ZipEntry` resources for each entry in the ZIP file. Here's an example:

```
val url = uri"http://example.com/test.zip"
val totalSie = url.unzip().map { entry =>
  entry.size[Byte]
}.sum
```

`ZipEntry`s are themselves resources, so all the usual Rapture streaming
operations may be used on them.

This feature was developed by Jorge FIXME.

## HTML

### Support for full HTML5 syntax (help)

Due to the vast number of them, many HTML5 tags and attributes were simply not
implemented in the last version of Rapture HTML. A more complete set has now
been included, so you can now write attributes like `placeholder` on `input`
tags, and `icon` on `command` tags, which was previously not possible. For
example,

```
Input(size = 20, name = 'phone, placeholder = "+44 1234 567890")
```

will produce the HTML tag

```
<input size="30" name="phone" placeholder="foobar"/>
```

Thanks to Sébastien Renault for this contribution!

### Support for nesting iterables inside elements

It is now possible to include any iterable collection of children within an
HTML element, alongside ordinary child elements. For example,

```
val items = Vector("two", "three", "four")
Ul(
  Li("First item"),
  items.map { item => Li(item) },
  Li("Last item")
)
```

Previously, the `: _*` varargs decorator had to be used carefully to include
collections of HTML nodes inside, and this suffered from various type-inference
problems. It should no longer ever be necessary to use `: _*`.

## JSON

### Serialization and extraction of maps

The last release of Rapture JSON had a bug when serializing and extracting
`Map`s from JSON. This has been fixed and enhanced. It is now possible to
serialize and extract maps of type `Map[K, V]`, provided the type `V` can be
serialized/extracted, and the type `K` can be serialized/deserialized as a
`String`.

```
val j = json"""{ "1": "one", "2": "two" }"""
val m = j.as[Map[Int, String]]
val j2: Json = Json(m)
```

### Easier creation of JSON instances from raw values

This will be of most use to JSON backend developers, but it's now much easier
to create a new `Json` value from its underlying representation.

```
import rapture.json._, jsonBackends.jawn._
val i = Json.raw(jawn.ast.JNum("1234567890"))
```

Note that the parameter to `raw` is inherently untyped, and only values which
the given backend knows how to work with should be provided here. It is
entirely possible to create new, unusable `Json` values this way. Some work is
planned to improve this situation, though.

### Bugfixes: serialization of string-like things

The bug where `Int`s and `Boolean`s (and other types for which `String`
converters exist) would be converted to `String`s when included in JSON
literals has been fixed.

### Improvements to JSON addition

The syntax for modifying immutable JSON values has changed, and now has better
support for working with array elements. You should now use the `copy` method
to create a new JSON value with one or more values changed. For example, to
change the `foo` element in the `bar` object to the number `42`, we write,

```
val j = json"""{ "bar": { "baz": true, "foo": "Hello World" } }"""
val updated = j.copy(_.bar.foo = 42)
```

and the `updated` value would be

```
json"""{ "bar": { "baz": true, "foo": 42 } }"""`
```

Like the `copy` method on case classes, `copy` in JSON can take any number of
arguments, and will create a new instance including all modifications, made in
order, e.g.

```
val updated = j.copy(_.foo = Map("key" -> value), _.quux = 42)
```

Any nodes which do not already exist will be automatically created.

## Internationalization

### Use of `&` instead of `|` for combining language strings

The combinator for combining language strings is now `&` instead of `|`, for example:

```
val msgs = en"Hello" & fr"Bonjour" & de"Guten Tag"
```

Thanks to the several people who suggested this change, and to Martin Odersky
for finally persuading me to do it.

### Google Translate support

As demonstrated in several talks on Rapture, support for [Google
Translate](https://translate.google.com/) has now been included in Rapture,
allowing you to translate messages at compile-time.

To use this feature, you must have a [Google Translate API
key](https://cloud.google.com/translate/v2/getting_started), which must be made
available to the compiler through an implicit, like so:

```
import rapture.i18n._, googleTranslate._
implicit val apiKey = GoogleApiKey("YOUR-API-KEY")
```

The compiler should then provide automatic translations for any languages which
have been omitted from the source, for example,

```
val msgs: I18n[String, En with De with Fr] = en"Hello"
```

## HTTP

### Fix for match errors on content types

This fix avoids runtime exceptions when POST requests are sent with a MIME type
other than `application/x-www-form-urlencoded`.

Thanks to Sébastien Renault for this!

## General

### Better JAR file layout for Eclipse sources

The format of published JAR files previously meant that Eclipse could not
previously find Rapture sources without a lot of extra work for the user.

The directory structure of the projects has now been rearranged to help Eclipse
find the sources.

### Removal of some reflection code

Some unnecessary code using Scala reflection in Rapture Core has been removed,
which eliminates some crashes on Android, and works better on Scala.JS.

## Thank you

As well as the contributors above, thanks must also go to Alistair Johnson,
Christian Pérez-Llamas, François Armand and Sam Halliday for SBT help,
design inspiration, bugreporting and bloody-minded enthusiasm, respectively.



