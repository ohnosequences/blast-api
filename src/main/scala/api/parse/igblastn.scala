package ohnosequences.blast.api.parse

import ohnosequences.blast.api.{ BlastOutputRecord, AnyOutputField, OutputField }
import ohnosequences.blast.api.outputFields._
import ohnosequences.cosas._, types._, klists._, records._

/*
  # IgBLAST output structure

  The only parseable IgBLAST output corresponds to `outfmt 7`; let's describe it.

  > **WARNING** this version works with the results of **one** query sequence

  ## Header

  Something like

  ```
  # IGBLASTN 2.6.0+
  # Query: JQ778271.1 Homo sapiens clone 71 TCR alpha chain mRNA, complete cds
  # Database: IG_DB/imgt.TR.Homo_sapiens.V.f.orf.p IG_DB/imgt.TR.Homo_sapiens.D.f.orf IG_DB/imgt.TR.Homo_sapiens.J.f.orf.p
  # Domain classification requested: imgt
  ```

  The essential bit here is the query ID: the line which starts with `# Query: `. If we take the query ID from the hit table (see below) we could forget about this header; that would be good.
*/
case object igblastn {

  type Line   = String
  type Field  = String
  type Header = String

  val tabSeparatedFields: Line => Seq[Field] =
    _.split("\t").map(_.trim)

  val groupFieldsWithHeaders: (Seq[Field], Seq[Header]) => Map[Header, Field] =
    (fields, headers) => (headers zip fields) toMap

  val isEmptyLine: Line => Boolean =
    _.isEmpty

  val isComment: Line => Boolean =
    _ startsWith "#"

  val regionFrom: (Region, Seq[Line]) => Seq[Line] =
    (region, lines) =>
      lines
        .dropWhile  { l => !region.startsAt(l)            }
        .takeWhile  { l => !region.endsAt(l)              }
        .filterNot  { l => isEmptyLine(l) || isComment(l) }

  case class Region(val startsAt: Line => Boolean, val endsAt: Line => Boolean)

  val vdjAnnotation: Region =
    Region(
      startsAt  = _ startsWith "# V-(D)-J rearrangement summary for query sequence",
      endsAt    = isEmptyLine
    )

  val vdjSequences: Region =
    Region(
      startsAt  = _ startsWith "# V-(D)-J junction details based on top germline gene matches",
      endsAt    = isEmptyLine
    )

  val cdr3Sequences: Region =
    Region(
      startsAt  = _ startsWith "# Sub-region sequence details",
      endsAt    = isEmptyLine
    )

  val vAnnotation: Region =
    Region(
      startsAt  = _ startsWith "# Alignment summary between query and top germline V gene hit",
      endsAt    = _ startsWith "Total"
    )

  val hitTable: Region =
    Region(
      startsAt  = _ startsWith "# Hit table",
      endsAt    = isEmptyLine
    )
}
