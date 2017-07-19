name := "pomelo"
organization := "pomelo"
version := "0.1-SNAPSHOT"

scalaVersion := "2.12.1"

fork := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Yinfer-argument-types", "-Xlint", "-Ypartial-unification", "-opt:_", "-opt-warnings:_")

libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % "1.7.0",
  "com.geirsson" %% "scalafmt-core" % "1.1.0",
  "com.geirsson" %% "scalafmt-cli" % "1.1.0",
  "com.chuusai" %% "shapeless" % "2.3.2",
  //"org.fxmisc.richtext" % "richtextfx" % "0.7-M5",
  "com.github.pathikrit" %% "better-files" % "2.17.1",
  "org.scala-sbt" %% "zinc" % "1.0.0-X10"
)

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies += "com.github.JordanMartinez" % "RichTextFX" % "jitpack-master-SNAPSHOT"

//mainClass in reStart := Some("pomelo.DevAppReloader")
