[![Build Status](https://travis-ci.org/propensive/rapture-core-scalaz.png?branch=master)](https://travis-ci.org/propensive/rapture-core-scalaz)
# Rapture Core/Scalaz

The Rapture Core/Scalaz project provides integration between Rapture Core and Scalaz 7. So far
this consists only of two additional return-type strategies:

 - A validation return-type strategy for returning Scalaz `Validation`s
 - A task return-type strategy, for returning Scalaz `Task`s

### Availability

#### Building from source

To build Rapture Core Scalaz from source, follow these steps:

```
git clone git@github.com:propensive/rapture-core-scalaz.git
cd rapture-core-scalaz
sbt package
```

If the compilation is successful, the compiled JAR file should be found in target/scala-2.10

### Return-Type Strategies

By importing `scalazStrategy.returnValidation`, every fallible method will return a result of
type `Validation[E, T]`.

```scala
> import scalazStrategy.returnValidation
> Json.parse("{}")
res: Validation[ParseException, Json] = {}
```

If using the standard `strategy.explicit` you can get a `Validation` by calling `.valid` on the
unexpanded result.

```scala
> import strategy.explicit
> Json.parse("{}").valid
res: Validation[ParseException, Json] = {}
```

Alternatively, by importing `scalazStrategy.returnTasks`, and providing a valid implicit
`ExecutorService`, every fallible method in Rapture will return a Scalaz `Task`.

```scala
> import scalazStrategy.returnTasks
> implicit val exec = scalaz.concurrent.Strategy.DefaultExecutorService
> Json.parse("{}")
res: Task[Json] = Task@7961b5f3
```

You can call `.task` on an unexpanded result when using `strategy.explicit` to get the same
effect.

See https://github.com/propensive/rapture-core#return-type-strategies for more information on
Rapture Core return-type strategies.
