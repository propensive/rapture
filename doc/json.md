# Rapture JSON

## Features

 - Clean, intuitive, unintrusive, boilerplate-free Scala API
 - Trivial extraction and serialization to/from primitives, collections and case classes
 - Consistent and typesafe type-class-based interfaces
 - Works with a choice of JSON parsers and backends
 - Simple, efficient conversion between different backends
 - Support for both immutable and mutable JSON
 - Flexible choice of error handling strategies using
   [modes](https://github.com/propensive/rapture-core)
 - Can be easily extended using composable user-defined extractors and
   serializers

# Using Rapture JSON

## JSON Representation

Rapture JSON is designed to be agnostic about the JSON parser and choice of AST
representation used throughout the library. This means that a choice of JSON
backend must be made in order to use Rapture JSON. Whilst different backend
libraries provide different features, all features of Rapture JSON are
available with every backend (with the exception of Jackson, which does not yet
support mutable JSON operations).

The choice of backend should therefore depend on other characteristics such as
performance, memory usage, required dependencies, integration with existing
libraries, licensing and policy choices.

The following backends are available:

 - Argonaut (`argonaut`)
 - Jackson (`jackson`)
 - Jawn (`jawn`)
 - JFC (`jfc`)
 - JSON4S (`json4s`)
 - Lift (`lift`)
 - Play (`play`)
 - Scala standard library JSON (`scalaJson`)
 - Spray (`spray`)

It is also possible to integrate with other JSON backends, though this is not
covered by this document. Anyone interested should look at the existing
integration type classes, and contact the Rapture mailing list or the Gitter
channel.

In the source code, you should import `rapture.json.jsonBackends.<backend>._`.

## The Json type

A JSON value, whether an array, object, boolean, number or string, is
represented by an instance of type `Json`. As JSON is inherently
dynamically-typed, the `Json` type is used to provide a safe and immutable
wrapper around the JSON tree, whose type is not known at compile time.

Instances of `Json` consist of three things:

 - a reference to the root of a dynamically-typed JSON tree
 - a path into a node within the JSON tree
 - a reference to the parser used to create, modify and read the JSON tree

Although using `Json` objects should seem very intuitive, it is important to
understand the purpose of this state.

```json
{
  "fruits": [
    {
      "name": "apple",
      "color": "red"
    },
    {
      "name": "banana",
      "color": "yellow"
    }
  ]
}
```

If we were to parse the above JSON source, we should get a tree consisting of
an object containing an array under the key "fruits", with two elements, each
of which is an object containing two fields, "name" and "color", both of which
are strings. Given this tree, we can refer to an element within with a path of
strings for indexing JSON objects, and integers for indexing JSON arrays, for
example, `fruits / 0 / name`, which would refer to the string `"apple"`.

We could also look into the same tree with the path `fruits / 3 / mass`, though
this wouldn't exist on account of there being only two elements in the `fruits`
list, but we would not know this until we attempted it at runtime.

A `Json` instance represents both the JSON tree, and a lazily-evaluated path
into that tree, which may or may not point to a value. If we assume the full
JSON tree is a starting point (most likely originating from being parsed from
source), `Json` instances can be created which hold the same reference to the
original tree, but point -- by means of a path -- to any subtree of the
original, without the performance cost of navigating the tree, or the
requirement to safely handle missing-value or type-mismatch errors which arise
because the path attempts to access a value which isn't available.

At some later point, if the JSON is to yield some useful data which we can do
interesting things with, we will need to perform the access, and assign a Scala
type to it, as it passes from the dynamic to the static world. It is at this
point that all access failures will arise, so by deferring them to a single
point, they can be handled just once.

Additionally, every `Json` instance stores a reference to the backend which was
used to create it, and which will be used to access it. As Rapture JSON permits
multiple different parsers to be used alongside each other, it is important
that the AST within each `Json` instance is always handled using the right
backend.

## Accessing JSON values

`Json` instances implement Scala's `Dynamic` trait, providing a very natural
way to refer to object fields within a `Json` value just by calling that field
name as if it were a method on the `Json` instance. Additionally, integers may
be applied to index into arrays.

For example, using the example JSON above, we can create a new `Json` instance
pointing to the string `"yellow"` as follows:

```scala
json.fruits(1).color
```

Remember, this is just creating a pointer into the `"yellow"` value; it's not
been accessed yet.  To extract a value from a `Json` value, the `as` method is
used. `as` takes a single type parameter, and is the single point at which a
JSON type-mismatch or missing-value exception can occur.

```scala
json.fruits(1).color.as[String]
```

Rapture JSON uses Rapture Core's modes on the `as` method, thus allowing
failure cases to be handled using the preferred exception-handling strategy,
for example by throwing an exception or returning a `Try`. See the section on
error messages below.

Note that calling `toString`, as happens automatically after every evaluation
in the Scala REPL, *will* cause the AST to be accessed, but any errors will be
suppressed, and the `toString` method will return the string `"undefined"`.

A variety of types may be extracted from a `Json` value, including the
following:

 - Primitive types, such as `String`, `Int`, `Double` and `Boolean`
 - Scala collections of other extractable types, e.g. `List[Int]` or `Set[Int]`
 - Case classes, extracted by the names of their parameters, provided the type
   of every parameter is extractable
 - `Option`s, where `None` will be extracted if the element is missing or the
   wrong type
 - `Json` types -- the no-op extractor
 - Any other type for which an implicit `Extractor` exists in scope

These types compose, so it is possible to extract values of complex types like
`Option[Vector[MyCaseClass]]`.

## Creating JSON values

JSON values can be created in a number of different ways. If starting with a
JSON source, which will often be a `String` (but may be another type -- the
Jawn backend can parse directly from ByteBuffers, for example), we can call
`Json.parse(src)` to attempt to parse the source `src`.

`Json` values can also be created directly in code, using the `json` string
context, like so:

```scala
json"""{
  "fruit": "apple",
  "variants": ["cox", "braeburn"]
}"""
```

Much like an interpolated string, Scala expressions may be substituted into a
`json` string context, provided the expressions evaluate to a type which is
serializable to Json. Generally speaking, all types which can be extracted from
a `Json` value (primitives, collections, case classes, Json) can also be serialized,
like so:

```scala
val f = "apple"
json"""{
  "fruit": $f,
  "variants": ${List("cox", "braeburn")}
}"""
```

Any serializable type can also be converted to `Json` by applying it to the
`Json` object, for example this,

```scala
case class Fruit(name: String, variants: Set[String])
Json(Fruit("apple", Set("cox", "braeburn")))
```

will produce the same JSON as the previous examples.

An instructive compile error will be displayed in the event of an attempt to
serialize a type which cannot be serialized to `Json`.

## Pattern matching on JSON

An alternative way of extracting values from `Json` types in to use pattern
matching. A `Json` value can be pattern matched against JSON literals defined
inline in the case clause, like this:

```scala
val json: Json = Json.parse(src)
json match {
  case json""" { "fruit": $name }""" =>
    name.as[String]
  case json""" { "vegetable": $name }""" =>
    name.as[String]
}
```

This will first attempt to match any JSON object which contains a key called
`fruit`, and bind the value to the identifier `name`. If that match fails, it
will attempt to match any JSON object which contains a key called `vegetable`,
and likewise bind its value to `name`. In each case, we return the name as a
`String`. The call to `.as[String]` is necessary because, as before, the
compiler will no know nothing about the nature of the type at compile time, so
it must be explicitly specified.

Literal match values may also be explicitly specified, for example:

```scala
val json: Json = Json.parse(src)
json match {
  case json""" { "fruit": "apple", "variety": $vs }""" =>
    vs.as[Set[String]]
  case json""" { "fruit": "lemon" }""" =>
    Set()
}
```

Multiple values may be extracted, and the pattern match expression may involve
arbitrarily-deep object and array nesting.

The examples above use the default configuration for structural pattern
matching, however three configuration explicits control the strictness of
structural pattern matching.

 - `patternMatching.exactObjects._`
 - `patternMatching.exactArrays._`
 - `patternMatching.exact._`

If it is required that pattern matching on JSON objects should match the entire
object, i.e. the existence of any keys in the object which are not specified in
the pattern should result in failure to match, then include the following
import somewhere within the scope of the pattern match:

```scala
import patternMatching.exactObjects._
```

By default, array elements specified in a pattern are matched positionally, and
superfluous array elements in the tail of the array are ignored, and the match
will be successful. If strict array matching is required, include:

```scala
import patternMatching.exactArrays._
```

If exact matching of both arrays and objects is required, then it is sufficient
to import

```scala
import patternMatching.exact._
```

## Modifying `Json`

Whatever underlying backend is used, the `Json` type is immutable. A small
number of methods are provided to create new `Json` values from existing
values. Given a `Json` value, a new key may be added using the following
syntax:

```scala
val j = json"""{ "fruit": "plum" }"""
j + (_.color, "purple")
```

This syntax is designed to resemble the syntax for adding a value to a
`scala.collection.Map`, whereby the tuple passed to the `+` operator contains a
key and a value, resulting in the addition of this value to the map. However,
with `Json` types, the "key" is a path into the `Json` structure, and the
"value" is the value to be serialized to JSON and stored at that position in
the JSON tree.

The `++` operator can be used to combine two JSON structures, like so:

```scala
val j1 = json"""{ "fruit": { "name": "grape" } }"""
val j2 = json"""{ "fruit": { "color": "white" } }"""
val json = j1 ++ j2
```

The resultant `json` value would be

```json
{
  "fruit": {
    "name": "grape",
    "color": "white"
  }
}
```

Currently, when merging, object keys from both sides will be merged, favoring
the right side in the event of a clash, and arrays will be concatenated. If the
types of corresponding values do not match, the value from the right side will
clobber that from the left. In a later version of Rapture JSON, more control
over mechanisms for combining and merging `Json` values may be provided through
configuration implicits.

## Error messages

By default, failures in Rapture JSON operations will result in an exception
being thrown, however, alternative exception handling methods can be used with
a simple import from the Rapture Core project which will determine the return
type of all fallible methods such as `Json.parse` and `as`. For example,

```scala
import rapture.core._
import modes.returnTry._
Json.parse(src)
```

will result in the return type of `Json.parse` changing from `Json` to
`Try[Json]`, safely capturing any failures which may result from the operation.

All Rapture JSON methods will throw a limited set of possible exceptions, which
are implemented as case classes inheriting from a sealed trait. If choosing to
use the `captureExceptions` mode, or another mode which captures the exception
and tracks the exception type in its signature, this allows exhaustivity
checking to be performed on errors, for example:

```scala
import modes.returnEither._
json.fruit.as[String] match {
  case Right(f) =>
    s"Found fruit $f"
  case Left(TypeMismatchException(found, _, _)) =>
    s"Fruit was the wrong type: $found"
  case Left(MissingValueException(path)) =>
    s"Fruit value was missing at path $path"
}
```

If either of the final two case clauses were omitted from this pattern match,
the compiler will issue a warning that the match may not be exhaustive.

## Mutable JSON

In the same way that the `List` type from the Scala collections library has a
corresponding mutable `ListBuffer` type, the `Json` type has a corresponding
`JsonBuffer` type, which supports mutability in addition to the operations
described above. The functionality of `JsonBuffer` is a strict superset of the
functionality of `Json`.

An empty `JsonBuffer` may be created with

```scala
val jb = JsonBuffer.empty
```

and can be mutated with instructions such as

```scala
jb.fruit.name = "apple"
jb.fruit.color = "green"
jb.fruit.varieties = List("cox")
```

resulting in

```json
{
  "fruit": {
    "name": "apple",
    "color": "green",
    "varieties": ["cox"]
  }
}
```

Note that the top-level `fruit` object gets automatically created by the first
instruction, and that the right-hand side of the assignment is serialized to
JSON using Rapture's standard serialization scheme, causing a compile-time
error if the type cannot be serialized.

It is additionally possible to append items to the end of an array using the
`+=` operator, like this:

```scala
jb.fruit.varieties += "braeburn"
```

Mutable JSON is not yet available for all backends, though this work is in
progress. Note that the underlying JSON backend does not need to be inherently
mutable to support mutable JSON. If a backend which uses an immutable AST is
used, Rapture JSON will efficiently perform the necessary tree manipulations,
updating references as necessary to give the impression of a mutable data
structure. However, using a backend which uses a mutable JSON representation
will likely result in better performance.

## Converting JSON

All JSON values are represented by the same Scala type, `Json`, regardless of
whether they represents a JSON object, array, string, number or boolean, and
regardless of which backend was used to create it.

In order to combine two `Json` values, which may be represented using different
ASTs, it is sometimes necessary to convert the internal representations of the
JSON values from one backend to another. In all cases, this happens seamlessly
when required.

For example, given the two values `j1` and `j2`,

```scala
import jsonBackends.jawn._
val j1: Json = json"""{ "foo": "bar" }"""
import jsonBackends.jfc._
val j2: Json = json"""{ "baz": "quux" }"""
```

the two JSON objects can be merged with `j1 ++ j2`. This will traverse the AST
of the value `j2`, converting it from a JFC to a Jawn representation, and then
merge it with the value `j1` to produce a `Json` value, represented as a Jawn
AST.

Likewise, substitutions into other JSON values will invoke conversions, as
necessary. So, given the previous two values `j1` and `j2`, we could substitute
both into a new JSON object (represented by some third backend AST), and each
will be converted to the new AST representation.

```scala
import jsonBackends.argonaut._
val j3 = json"""{ "j1": $j1, "j2": $j2 }"""
```

Sometimes it is useful to eagerly force conversion of a `Json` value to the
current JSON backend. This can be achieved in one of two ways:

 - extract a `Json` type from the existing `Json` value, i.e.
   `jsonValue.as[Json]`
 - construct a new `Json` value from the existing `Json` value, i.e.
   `Json(jsonValue)`

These operations are equivalent, and usage is a matter of personal taste. If
the value `jsonValue` is already represented by the currently-scoped JSON
backend, these operations will be no-ops.

Given that conversion happens seamlessly, on demand, the detail of which AST is
used to represent the JSON is abstracted away from the user. It can, however,
be revealed in a couple of ways:

 - extracting an `Any` from the `Json` value using `jsonValue.as[Any]`; this
   returns the underlying representation of the JSON value
 - getting the `JsonAst` from the `Json` value using `Json.ast(jsonValue)`

## Outputting JSON

Often, the easiest way to output JSON from the `Json` type is to call
`.toString` on the `Json` value. Although simple, this has the disadvantages
that it does not offer any flexibility in how the JSON is formatted, and it
always returns a `String` whereas other types, such as an input stream may be
more appropriate for some applications.

The more general method is to use the `Json.format` method, with an appropriate
implicit `Formatter` in scope. Two formatters are provided as standard:

 - `formatters.humanReadable._`, which formats the JSON with newlines and
   indentation, attempting to make it as readable as possible,
 - `formatters.compact._`, which includes no unnecessary whitespace

Both formatters return `String`s, though it is possible for other backends to
provide their own formatters for outputting to other types.

For example,

```scala
import formatters.compact._
val out = Json.format(json)
```

## Defining custom extractors and serializers

In order to be able to extract or serialize values of a particular Scala type,
`T` to or from JSON, an implicit instance of `Extractor[T]` or `Serializer[T]`
must be available in scope.

### Extractors

The easiest way to define an `Extractor` for a particular type is to start with
an existing extractor, e.g. the `String` extractor, by summoning it with
`Json.extractor[String]`. Extractors are functors, so you can `map` across the
extractor to produce a new extractor for a different type. For example, to
create an extractor for `java.util.Date`s, where these are stored as numbers in
the JSON, we can write:

```scala
implicit val dateExtractor = Json.extractor[Long].map(new java.util.Date(_))
```

and it then becomes possible to extract a date, e.g.

```scala
val j = json"""{ "start": 1438967950000 }"""
val d = j.start.as[java.util.Date]
```

Often, the most useful starting point for defining a new extractor is the no-op
`Json` extractor, `Json.extractor[Json]`. This allows definitions such as:

```scala
implicit val rangeExt = Json.extractor[Json].map { j =>
  Range(j.start.as[Int], j.end.as[Int])
}
```

or

```scala
implicit val msgExtractor = Json.extractor[Json].map {
  case json"""{ "ignore": true }""" =>
    None
  case json"""{ "message": $msg, "ignore": false }""" =>
    Some(Message(str.as[String]))
}
```

### Serializers

Likewise, `Serializer`s are cofunctors, so new serializers may be created by
`contramap`ping across an existing serializer. We can summon the serializer for
a type `T` with `Json.serializer[T]`. To produce a serializer for a type `S`
from this serializer, we need to provide the `contramap` method with a function
`S => T`, though because Scala's type system can't infer the type `S` from a
function literal, we need to specify this type explicitly to the `contramap`
method.

Here's an example:

```scala
implicit val exceptionSerializer = Json.serializer[String].contramap[Exception] { e =>
  s"${e.getClass.getName}: ${e.getMessage}"
}
```

The existence of this implicit serializer will then result in any `Exception`
being serialized to a representative `String`, anywhere it needs to be
converted into a `Json` value.

As with `Extractor`s, the `Json` serializer is often a useful starting point:

```scala
implicit val eitherSerializer = Json.serializer[Json].contramap[Either[String, Int]] {
  case Left(s) =>
    json"""{ "branch": "left", "value": $s }"""
  case Right(i) =>
    json"""{ "branch": "right", "value": $i }"""
}
```

### Fallback extractors

Extractors may sometimes encounter JSON data that doesn't fit their expected
pattern, and result in failure. It can be useful to provide alternative
extractors for JSON in the event that the first one fails. Extractors can be
composed in this way using the `orElse` combinator.

For example, the following extractor redefines the `Int` extractor to also
accept integers provided as JSON strings as a fallback option:

```scala
implicit val intExt = Json.extractor[Int] orElse Json.extractor[String].map(_.toInt)
```

Hopefully this demonstrates the concise and readable syntax Rapture JSON uses
for defining extractors and serializers, and the ease with which they can be
composed and transformed.

