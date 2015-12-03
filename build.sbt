Nice.scalaProject

name          := "blast"
organization  := "ohnosequences"
description   := "A typesafe Scala BLAST API"

scalaVersion  := "2.11.7"

libraryDependencies ++= Seq(
  "ohnosequences"         %% "cosas"      % "0.8.0-SNAPSHOT",
  "ohnosequences"         %% "datasets"   % "0.2.0-new-cosas-SNAPSHOT",
  "org.scalatest"         %% "scalatest"  % "2.2.5" % Test,
  "com.github.tototoshi"  %% "scala-csv"  % "1.2.2" % Test
)

// scalacOptions ++= Seq("-Xlog-implicits")
incOptions := incOptions.value.withNameHashing(false)

bucketSuffix  := "era7.com"
