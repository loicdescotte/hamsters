# Hamsters

A mini Scala utility library. Compatible with functional programming beginners. For the JVM and Scala.js.

<img src="https://raw.githubusercontent.com/scala-hamsters/hamsters/gh-pages/hamsters.jpg" alt="hamster logo" width="150px"/>

Currently, Hamsters supports :

 * Validation
 * Monad transformers
 * Future squash operations
 * Enum typeclass
 * Default values for options (orEmpty)
 * HLists
 * Union types
 * NonEmptyList
 * Show
 * mapN
 * Lenses
 * Memoization
 * Retry

## Documentation

[3.0.0 documentation](https://github.com/scala-hamsters/hamsters/tree/3.0.0/docs)  

[Master documentation](https://github.com/scala-hamsters/hamsters/tree/master/docs)

[![Travis](https://travis-ci.org/scala-hamsters/hamsters.svg?branch=master)](https://travis-ci.org/scala-hamsters/hamsters)

## Install as dependency

With SBT :

```scala
libraryDependencies ++= Seq(
  "io.github.scala-hamsters" %% "hamsters" % "3.0.0"
)
```

With Maven :

```xml
<dependency>
  <groupId>io.github.scala-hamsters</groupId>
  <artifactId>hamsters_${scala.version}</artifactId>
  <version>3.0.0</version>
</dependency>
```

For Scala.js :

```scala
libraryDependencies ++= Seq(
  "io.github.scala-hamsters" %%% "hamsters" % "3.0.0"
)
```

## Try Hamsters in ScalaFiddle or Ammonite

You can try Hamsters quickly and easily with ScalaFiddle.  
Example : [https://scalafiddle.io/sf/BDm8W4U/0](https://scalafiddle.io/sf/BDm8W4U/0)

Alternatively, a REPL script is provided. `try-hamster.sh` script downloads and installs 
[Coursier](https://github.com/alexarchambault/coursier), [Ammonite](https://github.com/lihaoyi/Ammonite) REPL and Hamsters dependency. 
No dependencies needed other than a JDK.

PS: The macros don't work with the REPL

```shell
curl -s https://raw.githubusercontent.com/scala-hamsters/hamsters/master/scripts/try-hamsters.sh | bash

Loading...
Welcome to the Ammonite Repl 1.0.1
(Scala 2.12.3 Java 1.8.0_151)

@ val noneString: Option[String] = None 
noneString: Option[String] = None

@ noneString.orEmpty 
res1: String = ""

@ val hlist1: Double :: String :: HNil = 2.0 :: "hi" :: HNil 
hlist1: Double :: String :: HNil = HCons(2.0, HCons("hi", HNIL))

@ exit 
Bye!

```

## Extensions

See [hamsters-extensions](https://github.com/scala-hamsters/hamsters-extensions) for more information.

## Scaladoc

You can find the API documentation [here](https://static.javadoc.io/io.github.scala-hamsters/hamsters_2.12/3.0.0/io/github/hamsters/index.html).

## Special thanks

To [Laurencebeillaux](https://github.com/laurencebeillaux) who created the Hamsters logo.
