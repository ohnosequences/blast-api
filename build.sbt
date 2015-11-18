Nice.scalaProject

name          := "blast"
organization  := "ohnosequences"
description   := "A typesafe Scala BLAST API"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "ohnosequences"         %% "cosas"      % "0.7.1",
  "ohnosequences"         %% "datasets"   % "0.2.0-M2",
  "org.scalatest"         %% "scalatest"  % "2.2.5" % Test,
  "com.github.tototoshi"  %% "scala-csv"  % "1.2.2" % Test
)
