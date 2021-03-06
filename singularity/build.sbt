scalaVersion := "2.13.1"

name := "singularity"
organization := "com.jacekduszenko"
version := "0.0.2"

// regular deps
libraryDependencies += "org.typelevel"          %% "cats-core"                % "2.0.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.typelevel"          %% "cats-core"                % "2.0.0"
libraryDependencies += "com.chuusai"            %% "shapeless"                % "2.3.3"
libraryDependencies += "org.javassist"          % "javassist"                 % "3.27.0-GA"
libraryDependencies += "org.typelevel"          %% "cats-effect"              % "2.2.0"
libraryDependencies += "com.beachape"           %% "enumeratum"               % "1.6.1"

//test deps
libraryDependencies ++= Seq(
  "org.scalatest"     %% "scalatest"                % "3.3.0-SNAP2" % "test",
  "org.scalacheck"    %% "scalacheck"               % "1.14.1"      % "test",
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % "test"
)
