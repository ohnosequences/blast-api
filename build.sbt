name          := "blast-api"
organization  := "ohnosequences"
description   := "A typesafe Scala BLAST API"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "ohnosequences"         %% "cosas"        % "0.8.0",
  "com.github.tototoshi"  %% "scala-csv"    % "1.3.4" % Test
)

// incOptions := incOptions.value.withNameHashing(false)

wartremoverErrors in (Test,    compile) := Seq()
