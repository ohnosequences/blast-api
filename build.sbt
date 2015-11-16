Nice.scalaProject

name          := "blast"
organization  := "ohnosequences"
description   := "A typesafe Scala BLAST API"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "ohnosequences"         %% "cosas"      % "0.8.0-SNAPSHOT",
  "ohnosequences"         %% "datasets"   % "0.2.0-SNAPSHOT",
  "org.scalatest"         %% "scalatest"  % "2.2.5" % Test,
  "com.github.tototoshi"  %% "scala-csv"  % "1.2.2" % Test
)
