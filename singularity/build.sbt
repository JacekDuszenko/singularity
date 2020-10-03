// The simplest possible sbt build file is just one line:

scalaVersion := "2.13.1"

name := "singularity"
organization := "com.jacekduszenko"
version := "0.0.1"

// regular deps
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"

//test deps
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "latest.integration" % "test"
)
