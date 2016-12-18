name          := "blast-api"
organization  := "ohnosequences"
description   := "A typesafe Scala BLAST API"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "ohnosequences"         %% "cosas"        % "0.8.0",
  "com.github.pathikrit"  %% "better-files" % "2.13.0",
  "com.github.tototoshi"  %% "scala-csv"    % "1.2.2" % Test
)

// incOptions := incOptions.value.withNameHashing(false)

wartremoverErrors in (Test,    compile) := Seq()
