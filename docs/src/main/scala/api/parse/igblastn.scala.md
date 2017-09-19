
```scala
package ohnosequences.blast.api.parse

import ohnosequences.blast.api._, ohnosequences.blast.api.igblastn.output._
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
    _.split('\t').map(_.trim)

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

  /** === Summary total numbers ===
    *
    * @param queries          Total queries
    * @param identifiableCDR3 Total identifiable CDR3
    * @param uniqueClonotypes Total unique clonotypes
    */
  case class Totals(
    queries: Int,
    identifiableCDR3: Int,
    uniqueClonotypes: Int
  ) {

    def toSeq: Seq[Int] = Seq(
      queries,
      identifiableCDR3,
      uniqueClonotypes
    )

    def toTSV: String = toSeq.mkString("\t")
  }

  case object Totals {

    /** [[Totals]] fields names that can be used as a DSV header */
    val DSVHeader: Seq[Field] = Seq(
      "Total queries",
      "Total identifiable CDR3",
      "Total unique clonotypes"
    )

    val TSVHeader: String = DSVHeader.mkString("\t")

    def parseFromLines(lines: Iterator[String]): Option[Totals] = {
      def getIntVal(line: String): Int = line.split('=').map(_.trim).last.toInt

      val fields: Seq[Int] = lines
        .dropWhile { line => line.isEmpty || !line.startsWith("Total") }
        .take(3).toSeq
        .map(getIntVal)

      if (fields.length != 3) None
      else Some {
        Totals(
          fields(0),
          fields(1),
          fields(2)
        )
      }
    }
  }

  /** === Clonotype summary ===
    *
    * A particular clonotype includes any V(D)J rearrangements that have the same germline V(D)J gene segments, the same productive/non-productive status and the same CDR3 nucleotide as well as amino sequence (Those having the same CDR3 nucleotide but different amino acid sequence or productive/non-productive status due to frameshift in V or J gene are assigned to a different clonotype.  However, their clonotype identifers share the same prefix, for example, 6a, 6b).
    *
    * @param id         clonotype identifier
    * @param repSeqId   representative query sequence name
    * @param count      count
    * @param freqPerc   frequency (%)
    * @param cdr3Nuc    CDR3 nucleotide sequence
    * @param cdr3aa     CDR3 amino acid sequence
    * @param productive productive status
    * @param chainType  chain type
    * @param Vgene      V gene
    * @param Dgene      D gene
    * @param Jgene      J gene
    */
  case class ClonotypeSummary(
    id          : String,
    repSeqId    : String,
    count       : Int,
    freqPerc    : Double,
    cdr3Nuc     : String,
    cdr3aa      : String,
    productive  : Boolean,
    chainType   : ChainTypes,
    Vgene       : Seq[String],
    Dgene       : Seq[String],
    Jgene       : Seq[String]
  ) {

    /** Transforms [[ClonotypeSummary]] to a sequence of strings prepared for further DSV formatting. Fields strings are _not escaped_. */
    def toSeq: Seq[String] = Seq(
      id,
      repSeqId,
      count.toString,
      freqPerc.toString,
      cdr3Nuc,
      cdr3aa,
      productive.toString,
      chainType.toString,
      Vgene.mkString(","),
      Dgene.mkString(","),
      Jgene.mkString(",")
    )

    /** Formats [[ClonotypeSummary]] as a TSV (tab separated) string */
    def toTSV: String = toSeq.mkString("\t")
  }

  case object ClonotypeSummary {

    /** [[ClonotypeSummary]] fields names that can be used as a DSV header */
    val DSVHeader: Seq[Field] = Seq(
      "IgBLAST clonotype identifier",
      "Representative query sequence name",
      "Count",
      "Frequency (%)",
      "CDR3 nucleotide sequence",
      "CDR3 amino acid sequence",
      "Productive status",
      "Chain type",
      "V",
      "D",
      "J"
    )

    val TSVHeader: String = DSVHeader.mkString("\t")

    // FIXME: a lot can fail here, proper error management would be good
    def fromSeq(fields: Seq[String]): Option[ClonotypeSummary] = {
      def ifPresent(s: String): Seq[String] = {
        if(s == "N/A") Seq()
        else s.split(',').map(_.trim).toSeq
      }

      if(fields.length != 11) None
      else ChainTypes.parse(fields(7)).map { chainType =>
        ClonotypeSummary(
          id         = fields(0),
          repSeqId   = fields(1),
          count      = fields(2).toInt,
          freqPerc   = fields(3).toDouble,
          cdr3Nuc    = fields(4),
          cdr3aa     = fields(5),
          productive = fields(6) == "Yes",
          chainType  = chainType,
          Vgene      = ifPresent(fields(8)),
          Dgene      = ifPresent(fields(9)),
          Jgene      = ifPresent(fields(10))
        )
      }
    }

    def parseFromLines(lines: Iterator[String]): Iterator[Option[ClonotypeSummary]] = {
      val relevantLines: Iterator[String] = lines
        .dropWhile { line => !line.startsWith("#Clonotype summary") }
        .drop(1)
        .dropWhile { _.isEmpty }
        .takeWhile { line => line.nonEmpty && !line.startsWith("#All query sequences grouped by clonotype") }

      relevantLines
        .map(_.split('\t').map(_.trim).toSeq)
        .map(fromSeq)
    }
  }

  implicit class ClonotypeSummariesOps(val summaries: TraversableOnce[ClonotypeSummary]) extends AnyVal {

    def toTSV: Iterator[String] = summaries.toIterator.map(_.toTSV)

    /** Filters only productive [[ClonotypeSummary]]s and normalizes their frequency percentage
      * @return a `Seq` of productive clonotype summaries, because it requires two traversals
      */
    def onlyProductive: Seq[ClonotypeSummary] = {
      val productive = summaries.filter(_.productive).toSeq
      val productiveCount: Double = productive.map(_.count).sum

      productive.map { cs =>
        cs.copy(freqPerc = cs.count * 100 / productiveCount)
      }
    }
  }

  /** === All query sequences grouped by clonotypes ===
    *
    * @param id              clonotype identifier
    * @param count           query sequence count
    * @param freqPerc        frequency (%)
    * @param minSimPercVgene min similarity to top germline V gene (%)
    * @param maxSimPercVgene max similarity to top germline V gene (%)
    * @param avgSimPercVgene average similarity to top germline V gene (%)
    * @param querySeqs       query sequence names
    * @param readsCount      summary reads count calculated from the query sequences' names
    */
  case class Clonotype(
    id              : String,
    count           : Int,
    freqPerc        : Double,
    minSimPercVgene : Double,
    maxSimPercVgene : Double,
    avgSimPercVgene : Double,
    querySeqs       : Seq[String],
    readsCount      : Int
  )

  case object Clonotype {

    def fromSeq(fields: Seq[String]): Option[Clonotype] = {
      if(fields.length != 7) None
      else Some {
        val querySeqs: Seq[String] =
          fields(6).split(',').map(_.trim).toSeq

        // In CCGAG:TGTGCTATGTTGAGGT:3:0.047884850241877504 first number
        // is the number of reads    ^
        val readsCount: Int =
          querySeqs.map { _.split(':')(2).toInt }.sum

        Clonotype(
          id              = fields(0),
          count           = fields(1).toInt,
          freqPerc        = fields(2).toDouble,
          minSimPercVgene = fields(3).toDouble,
          maxSimPercVgene = fields(4).toDouble,
          avgSimPercVgene = fields(5).toDouble,
          querySeqs       = querySeqs,
          readsCount      = readsCount
        )
      }
    }

    def parseFromLines(lines: Iterator[String]): Iterator[Option[Clonotype]] = {
      val relevantLines: Iterator[String] = lines
        .dropWhile { line => ! line.startsWith("#All query sequences grouped by clonotypes") }
        .dropWhile { _.startsWith("#All query sequences grouped by clonotypes") }
        .filter { _.nonEmpty }

      relevantLines
        .map(_.split('\t').map(_.trim).toSeq)
        .map(fromSeq)
    }
  }

}

```




[main/scala/api/commands/blastn.scala]: ../commands/blastn.scala.md
[main/scala/api/commands/blastp.scala]: ../commands/blastp.scala.md
[main/scala/api/commands/blastx.scala]: ../commands/blastx.scala.md
[main/scala/api/commands/igblastn.scala]: ../commands/igblastn.scala.md
[main/scala/api/commands/makeblastdb.scala]: ../commands/makeblastdb.scala.md
[main/scala/api/commands/tblastn.scala]: ../commands/tblastn.scala.md
[main/scala/api/commands/tblastx.scala]: ../commands/tblastx.scala.md
[main/scala/api/expressions.scala]: ../expressions.scala.md
[main/scala/api/options.scala]: ../options.scala.md
[main/scala/api/outputFields.scala]: ../outputFields.scala.md
[main/scala/api/package.scala]: ../package.scala.md
[main/scala/api/parse/igblastn.scala]: igblastn.scala.md
[test/scala/CommandGeneration.scala]: ../../../../test/scala/CommandGeneration.scala.md
[test/scala/igblastn.scala]: ../../../../test/scala/igblastn.scala.md
[test/scala/igblastnClonotypesOutput.scala]: ../../../../test/scala/igblastnClonotypesOutput.scala.md
[test/scala/OutputFieldsSpecification.scala]: ../../../../test/scala/OutputFieldsSpecification.scala.md
[test/scala/OutputParsing.scala]: ../../../../test/scala/OutputParsing.scala.md