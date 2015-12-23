# Rapture

[![Build Status](https://travis-ci.org/propensive/rapture.png?branch=dev)](https://travis-ci.org/propensive/rapture)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.propensive/rapture-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.propensive/rapture-core_2.11)
[![License](http://img.shields.io/:license-Apache%202-brightgreen.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)
[![Gitter](https://img.shields.io/badge/gitter-join%20chat-green.svg)](https://gitter.im/propensive/rapture)

Rapture is an evolving collection of *useful* libraries for solving common,
everyday programming tasks, using advanced features of Scala to offer better
type-safety through powerful APIs that all Scala developers, beginners and
advanced users, should find intuitive.

Rapture consists of a number of modules, the most notable of which are:

 - Core (`core`) — a library of common utilities for other projects, notably
   *modes* and the `Result` type
 - [JSON](doc/json.md) (`json`) — comprehensive support for working with JSON
   data
 - I/O (`io`) — I/O (network, filesystem) functionality and infrastructure
 - I18n (`i18n`) — simple, typesafe representation of internationalized strings
 - CLI (`cli`) — support for working with command-line applications and shell
   interaction

# Themes in Rapture

The Rapture modules share a common philosophy that has evolved over time and
experience. Here are a few of the philosophical themes crosscutting all of the
Rapture modules.

 - A primary goal of intuitive, readable APIs and minimal code repetition
 - Extreme type-safety, with a goal to reduce the surface area of code exposed
   to runtime exceptions
 - Thoroughly typeclass-driven design, for extensibility
 - Fearless exploitation of all Scala features, where (but only where) it is
   appropriate
 - Agnostic support for multiple, alternative implementations of many
   operations with pluggable backends
 - Extensive, but principled, usage of implicits to configure and constrain
   operations
 - Support for modes in most APIs; the ability to change how failures are
   handled through return types

## Availability

Snapshots of Rapture are available for Scala 2.10 and 2.11 under the *Apache
2.0 License* in the [Sonatype Snapshots
repository](https://oss.sonatype.org/content/repositories/snapshots/com/propensive/),
with group ID `com.propensive` and artifact ID `rapture-[module]`, where module
is the name of the module, as taken from the list above.

Development work to get most Rapture modules working on
[Scala.JS](htp://www.scala-js.org/) is ongoing.

You can build and run Rapture locally by cloning this repository and running
`sbt publishLocal`.

## Contributing

Rapture openly welcomes contributions! We would love to receive pull requests
of bugfixes and enhancements from other developers. To avoid potential wasted
effort, bugs should first be reported on the Github issue tracker, and it's
normally a good idea to talk about enhancements on the [Gitter
channel](https://gitter.im/propensive/rapture) before embarking on any
development.

Alternatively, just send Jon Pretty
([@propensive](https://twitter.com/propensive/)) a tweet to start a
conversation.

Current contributors include:

 - Jon Pretty
 - Raúl Raja Martínez
 - Alistair Johnson

## Documentation

Rapture's documentation is currently sparse, though we are working to improve
this.

