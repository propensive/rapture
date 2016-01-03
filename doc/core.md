[![Build Status](https://travis-ci.org/propensive/rapture-core.png?branch=master)](https://travis-ci.org/propensive/rapture-core)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/propensive/rapture?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# Rapture Core

The Rapture Core project provides a common foundation upon which other Rapture projects are
based, however it provides utilities which may be useful in any project. Namely,

 - Modes (previously called return-type strategies)
 - A lightweight abstraction on time libraries, and implementations for Java time
 - A tiny (but growing) collection of conveniences for working within the REPL
 - An alias for `implicitly`
 - A really simple implementation of actors, based on `scala.concurrent`
 - Reusable string serializers and deserializers, with implicit-based configurable number formatting
 - Miscellaneous other small tools and utilities

### Availability

Rapture Core 1.1.0 is available under the Apache 2.0 License from Maven Central with group ID `com.propensive` and artifact ID `rapture-core_2.10`.

#### SBT

You can include Rapture Core as a dependency in your own project by adding the following library dependency to your build file:

```scala
libraryDependencies ++= Seq("com.propensive" %% "rapture-core" % "1.1.0")
```

#### Maven

If you use Maven, include the following dependency:

```xml
<dependency>
  <groupId>com.propensive</groupId>
  <artifactId>rapture-core_2.11</artifactId>
  <version>1.1.0<version>
</dependency>
```

#### Download

You can download Rapture Core directly from the [Rapture website](http://rapture.io/)
Rapture Core depends on Scala 2.11, but has no other dependencies.

#### Building from source

To build Rapture Core from source, follow these steps:

```
git clone -b scala-2.11 git@github.com:propensive/rapture-core.git
cd rapture-core
sbt package
```

If the compilation is successful, the compiled JAR file should be found in target/scala-2.11.0-RC4

### Modes

Rapture's modes allow library methods to be written in such a way that they may
be wrapped in another function (and have a different return type) at the call site. This
pattern allows *users* of the library to choose the return type and additional pre- and
post-execution processing to be performed, depending on their needs.  For example, using Rapture
JSON, given one of the imported modes,

```scala
import modes.returnEither._
Json.parse("[1, 2, 3]")
```

will have return type `Either[ParseException, Json]`. Hopefully the parsing succeeded, and the
return type will be `Right[Json]` rather than `Left[ParseException]`.

Alternatively, given a different imported mode, we will get a different return type.

```scala
import modes.returnFuture._
Json.parse("[1, 2, 3]")
```

This will immediately return a `Future[Json]`, from which the result can be obtained once
processing completes.

A selection of modes are provided:

- `modes.throwExceptions._` - does no additional processing, and simply returns the value,
  leaving any thrown exceptions unhandled. This is the default.
- `modes.returnEither._` - captures successful results in the `Right` branch of an
  `Either`, or exceptions in the `Left` branch.
- `modes.returnOption._` - returns an `Option` of the result, where the exceptional case
  collapses to `None`.
- `modes.returnTry` - wraps the result in a `scala.util.Try`.
- `modes.returnFuture._` - wraps the result in a `scala.concurrent.Future`; requires an
  implicit ExecutionContext.
- `modes.timeExecution._` - times the duration of carrying out the execution, returning a tuple
  of the return value and the time taken; requires an implicit `rapture.core.TimeSystem`.
- `modes.keepCalmAndCarryOn._` - catches exceptions and silently returns them as
  null; this is strongly discouraged!
- `modes.explicit._` - returns an instance of `Explicit` which requires the mode
  to be explicitly specified at the call site every time.

Multiple modes can be composed, should this be required, for example,

```scala
implicit val handler = modes.returnTry compose modes.timeExecution
```

#### The `modally` method

The `modally` method takes a lambda and evaluates it in the current mode, e.g.

```scala
import modes.returnTry._
modally { Class.forName(className) }
// Returns a Try[Class[_]]
```

This may provide a convenient (and adaptive) alternative to `try`/`catch`.

#### Writing methods to use modes

To transform a method like this,

```scala
def doSomething[T](arg: String, arg2: T): Double = {
  // method body
}
```

into one which offers end-users a choice of mode, include an implicit
Mode parameter, and wrap your method body and return type, like this,

```scala
def doSomething[T](arg: String, arg2: T)(implicit mode: Mode[_]):
    mode.Wrap[Double, Exception] = mode wrap {
  // method body
}
```

If you know that your method body will only throw exceptions of a certain type, you can
specify this in the method return type in place of `Exception`, which may make processing
exceptions easier when using a mode which captures them (e.g. `modes.returnEither`).

#### Conveniences for existing methods

You may already have existing implementations of methods you would like to modify to utilize
modes, but which already have a return type which captures (or discards) the exception
rather than throwing it. The `wrapEither`, `wrapTry` and `wrapOption` methods are provided
as alternatives to `wrap` to transform methods which already return `Either`, `Try` or
`Option` respectively. Note, however, that because `wrapOption` loses its exception in the
return type, that there is no way to recover it. This means that when using it with modes
such as `returnEither`, that the exceptional cases will always return as a
`java.util.NoSuchElementException`.

### REPL utilities

When working within the REPL, it is useful to receive the immediate feedback that a fallible
operation has failed (as the `throwExceptions` mode provides), but seeing large and unwieldy
stack traces can be annoying.  By including the following import,

```scala
import rapture.core.repl._
```

stack traces from fallible Rapture methods will be suppressed, and a short error message
will be displayed instead. If you would like to view the stack trace, it can be rethrown by
calling `repl.lastException`.

### Time System Abstraction

Many APIs take parameters or return values which represent time. Unfortunately, there is no
standard for representing entities like instants and durations.  Rapture Core provides a general
type class for defining these types and methods for creating them, and provides two simple
implementations:

- `timeSystems.numeric` - uses `Long`s to represent both instants and durations.
- `timeSystems.javaUtil` - uses `java.util.Date`s to represent instants, and `Long`s to
  represent durations.

### Alias for `implicitly`

Context-bounds provide a nice, lightweight syntax for working with type classes in Scala,
however while explicitly specifying an implicit parameter necessarily provides a named handle
for that implicit, context-bounds force us to make repeated use of the `implicitly` method in
order to use the type class. This can make using context-bounds more cumbersome than they
deserve.

Rapture Core introduces an alias for `implicitly` named `?`. Generally speaking, any occurrence
of `implicitly` can be replaced by a `?`. This is particularly useful when calling methods which
take multiple implicit parameters and you would like to specify one of these explicitly. For
example, a method like this:

```scala
def performAction[T](v: Int)(implicit alpha: Alpha[T], beta: Beta, gamma: Gamma) = { ... }
```

may now be called quite concisely using

```scala
performAction(42)(?, ?, myGamma)
```

if we only wanted to specify the parameter `myGamma` explicitly.
