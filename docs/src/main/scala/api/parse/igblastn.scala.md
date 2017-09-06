
```scala
package ohnosequences.blast.api.parse

import ohnosequences.blast.api.{ BlastOutputRecord, AnyOutputField, OutputField }
import ohnosequences.blast.api.outputFields._
import ohnosequences.cosas._, types._, klists._, records._
```


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


```scala
case object igblastn {

  type Line   = String
  type Field  = String
  type Header = String

  case object TCRA {

    import ohnosequences.blast.api.igblastn.output.TCRA._

    def parseVJRearrangementSummary(lines: Seq[Line]) =
      regionFields(vdjAnnotation, lines)
        .map({ fields => groupFieldsWithHeaders(fields, VJRearrangementSummary.keys.types map typeLabel asList) })
        .map({ VJRearrangementSummary parse _ })

    def parseVJJunctionDetails(lines: Seq[Line]) =
      regionFields(vdjSequences, lines)
        .map({ fields => groupFieldsWithHeaders(fields, VJJunctionDetails.keys.types map typeLabel asList) })
        .map({ VJJunctionDetails parse _ })

    def parseCDR3Sequence(lines: Seq[Line]) = {

      val fields =
        regionFields(cdr3Sequences, lines)
          .map(_.tail) // ugly I know

      (if(fields.length > 2) fields take 2 else fields)
        .map({ fields => groupFieldsWithHeaders(fields, CDR3Sequence.keys.types map typeLabel asList) })
        .map({ CDR3Sequence parse _ })
    }

    def parseVRegionAnnotations(lines: Seq[Line]) =
      regionFields(vAnnotation, lines)
        .map({ fields => groupFieldsWithHeaders(fields, VRegionAnnotations.keys.types map typeLabel asList) })
        .map({ VRegionAnnotations parse _ })

    def parseHitTable(lines: Seq[Line]) =
      regionFields(hitTable, lines)
        .map({ fields => groupFieldsWithHeaders(fields, HitTable.keys.types map typeLabel asList) })
        .map({ HitTable parse _ })
  }

  case object TCRB {

    import ohnosequences.blast.api.igblastn.output.TCRB._

    def parseVDJRearrangementSummary(lines: Seq[Line]) =
      regionFields(vdjAnnotation, lines)
        .map({ fields => groupFieldsWithHeaders(fields, VDJRearrangementSummary.keys.types map typeLabel asList) })
        .map({ VDJRearrangementSummary parse _ })

    def parseVDJJunctionDetails(lines: Seq[Line]) =
      regionFields(vdjSequences, lines)
        .map({ fields => groupFieldsWithHeaders(fields, VDJunctionDetails.keys.types map typeLabel asList) })
        .map({ VDJunctionDetails parse _ })

    def parseCDR3Sequence(lines: Seq[Line]) = {

      val fields =
        regionFields(cdr3Sequences, lines)
          .map(_.tail) // ugly I know

      (if(fields.length > 2) fields take 2 else fields)
        .map({ fields => groupFieldsWithHeaders(fields, CDR3Sequence.keys.types map typeLabel asList) })
        .map({ CDR3Sequence parse _ })
    }

    def parseVRegionAnnotations(lines: Seq[Line]) =
      regionFields(vAnnotation, lines)
        .map({ fields => groupFieldsWithHeaders(fields, VRegionAnnotations.keys.types map typeLabel asList) })
        .map({ VRegionAnnotations parse _ })

    def parseHitTable(lines: Seq[Line]) =
      regionFields(hitTable, lines)
        .map({ fields => groupFieldsWithHeaders(fields, HitTable.keys.types map typeLabel asList) })
        .map({ HitTable parse _ })
  }

  case class Region(val startsAt: Line => Boolean, val endsAt: Line => Boolean)

  val regionFields: (Region, Seq[Line]) => Seq[Seq[Field]] =
    (region, lines) =>
      regionFrom(region, lines)
        .map(tabSeparatedFields)

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

```




[test/scala/CommandGeneration.scala]: ../../../../test/scala/CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: ../../../../test/scala/OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: ../../../../test/scala/OutputFieldsSpecification.scala.md
[test/scala/igblastn.scala]: ../../../../test/scala/igblastn.scala.md
[main/scala/api/outputFields.scala]: ../outputFields.scala.md
[main/scala/api/options.scala]: ../options.scala.md
[main/scala/api/package.scala]: ../package.scala.md
[main/scala/api/expressions.scala]: ../expressions.scala.md
[main/scala/api/parse/igblastn.scala]: igblastn.scala.md
[main/scala/api/commands/blastn.scala]: ../commands/blastn.scala.md
[main/scala/api/commands/blastp.scala]: ../commands/blastp.scala.md
[main/scala/api/commands/tblastx.scala]: ../commands/tblastx.scala.md
[main/scala/api/commands/tblastn.scala]: ../commands/tblastn.scala.md
[main/scala/api/commands/blastx.scala]: ../commands/blastx.scala.md
[main/scala/api/commands/makeblastdb.scala]: ../commands/makeblastdb.scala.md
[main/scala/api/commands/igblastn.scala]: ../commands/igblastn.scala.md