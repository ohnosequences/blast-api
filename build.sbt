Nice.scalaProject

name          := "blast-api"
organization  := "ohnosequences"
description   := "A typesafe Scala BLAST API"

scalaVersion  := "2.11.8"

libraryDependencies ++= Seq(
  "ohnosequences"         %% "cosas"        % "0.8.0",
  "com.github.pathikrit"  %% "better-files" % "2.13.0",
  "org.scalatest"         %% "scalatest"    % "2.2.5" % Test,
  "com.github.tototoshi"  %% "scala-csv"    % "1.2.2" % Test
)

incOptions := incOptions.value.withNameHashing(false)

bucketSuffix  := "era7.com"
