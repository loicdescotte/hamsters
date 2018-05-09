import sbt.Keys._
import sbtcrossproject.{crossProject, CrossType}


val globalSettings =Defaults.coreDefaultSettings ++ Seq(
  organization := "io.github.scala-hamsters",
  version := "2.6.0",
  scalacOptions ++= Seq(),
  scalacOptions in(Compile, doc) := Seq("-groups", "-implicits"),
  publishMavenStyle := true
)

val buildSettings = globalSettings ++ Seq(
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0" % Provided,
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full),
  scalacOptions ++= List("-Xplugin-require:macroparadise", "-language:higherKinds", "-language:implicitConversions", "-feature"),
  scalacOptions in(Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  resolvers += Resolver.bintrayIvyRepo("scalameta", "maven")
)


val noPublishSettings = Seq(
  publishArtifact := false,
  publish := {},
  publishLocal := {},
)

lazy val publishSettings = Seq(
  pomExtra := (
    <url>https://github.com/scala-hamsters/hamsters</url>
      <licenses>
        <license>
          <name>Apache 2.0</name>
          <url>http://www.apache.org/licenses/</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:scala-hamsters/hamsters.git</url>
        <connection>scm:git@github.com:scala-hamsters/hamsters.git</connection>
      </scm>
      <developers>
        <developer>
          <id>loicdescotte</id>
          <name>Loïc Descotte</name>
          <url>http://loicdescotte.github.io/</url>
        </developer>
        <developer>
          <id>dgouyette</id>
          <name>Damien Gouyette</name>
          <url>http://www.Cestpasdur.com/</url>
        </developer>
        <developer>
          <id>oraclewalid</id>
          <name>Walid Chergui</name>
          <url>https://github.com/oraclewalid</url>
        </developer>
      </developers>
    )
)

lazy val noDocFileSettings = Seq (
  sources in doc in Compile := List()
)

val hamstersSettings = buildSettings ++ publishSettings

scalaVersion in ThisBuild := "2.12.4"
crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.4")
publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.toLowerCase.endsWith("snapshot"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("staging" at nexus + "service/local/staging/deploy/maven2")
}

lazy val metas = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("metas"))
  .settings(hamstersSettings)
  .settings(noDocFileSettings)

lazy val metasJVM = metas.jvm.settings(name := "metas")
lazy val metasJS = metas.js.settings(name := "metas")


val buildMacrosSettings = globalSettings ++ Seq(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

val macroSettings = buildMacrosSettings ++ publishSettings

lazy val macros = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("macros"))
  .settings(buildMacrosSettings)
  .settings(noDocFileSettings)

lazy val macrosJVM = macros.jvm.settings(name := "macros")
lazy val macrosJS = macros.js.settings(name := "macros")

lazy val hamsters = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .dependsOn(metas)
  .dependsOn(macros)  
  .settings(libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % "3.0.4" % "test",
    "org.scalamock" %%% "scalamock-scalatest-support" % "3.6.0" % "test",
    "org.scalacheck" %%% "scalacheck" % "1.13.4" % "test",
    "ca.mrvisser" %% "sealerate" % "0.0.5" % "test"
  ))
  .settings(hamstersSettings)

lazy val hamstersJVM = hamsters.jvm.settings(name := "hamsters")
lazy val hamstersJS = hamsters.js.settings(name := "hamsters")

lazy val root = project.in(file("."))
  .aggregate(hamstersJVM, hamstersJS, metasJVM, metasJS, macrosJVM, macrosJS)
  .dependsOn(hamstersJVM, hamstersJS, metasJVM, metasJS, macrosJVM, macrosJS)
  .settings(hamstersSettings)
  .settings(noPublishSettings)