Nice.scalaProject

name          := "blast"
organization  := "ohnosequences"
description   := "A typesafe Scala BLAST API"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "ohnosequences"         %% "cosas"      % "0.7.0-SNAPSHOT",
  "ohnosequences"         %% "datasets"   % "0.1.0-SNAPSHOT",
  "org.scalatest"         %% "scalatest"  % "2.2.4" % Test,
  "com.github.tototoshi"  %% "scala-csv"  % "1.2.2" % Test
)