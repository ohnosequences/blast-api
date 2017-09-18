package ohnosequences.blast.api.parse

import ohnosequences.blast.api._, ohnosequences.blast.api.igblastn.output._
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

  case object clonotypes {

    // #Clonotype summary.  A particular clonotype includes any V(D)J rearrangements that have the same germline V(D)J gene segments, the same productive/non-productive status and the same CDR3 nucleotide as well as amino sequence (Those having the same CDR3 nucleotide but different amino acid sequence or productive/non-productive status due to frameshift in V or J gene are assigned to a different clonotype.  However, their clonotype identifers share the same prefix, for example, 6a, 6b).  Fields (tab-delimited) are clonotype identifier, representative query sequence name, count, frequency (%), CDR3 nucleotide sequence, CDR3 amino acid sequence, productive status, chain type, V gene, D gene, J gene
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
    )

    case object ClonotypeSummary {

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

      def toSeq(cs: ClonotypeSummary): Seq[String] = Seq(
        cs.id,
        cs.repSeqId,
        cs.count.toString,
        cs.freqPerc.toString,
        cs.cdr3Nuc,
        cs.cdr3aa,
        cs.productive.toString,
        cs.chainType.toString,
        cs.Vgene.mkString(","),
        cs.Dgene.mkString(","),
        cs.Jgene.mkString(",")
      )

      def toTSVLine(cs: ClonotypeSummary): String =
        toSeq(cs).mkString("\t")


      // a lot can fail here, proper error management would be good
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

      def toTSV(cs: Iterator[ClonotypeSummary]): Iterator[String] =
        cs.map(toTSVLine)
    }

    // #All query sequences grouped by clonotypes.  Fields (tab-delimited) are clonotype identifier, count, frequency (%), min similarity to top germline V gene (%), max similarity to top germline V gene (%), average similarity to top germline V gene (%), query sequence name (multiple names are separated by a comma if applicable)
    case class Clonotype(
      id              : String,
      count           : Int,
      freqPerc        : Double,
      minSimPercVgene : Double,
      maxSimPercVgene : Double,
      avgSimPercVgene : Double,
      querySeqs       : Seq[String],
      readCount       : Int
    )

    case object Clonotype {

      def fromSeq(fields: Seq[String]): Option[Clonotype] = {
        if(fields.length != 7) None
        else Some {
          val querySeqs: Seq[String] =
            fields(6).split(',').map(_.trim).toSeq

          // In CCGAG:TGTGCTATGTTGAGGT:3:0.047884850241877504 first number
          // is the number of reads    ^
          val readCount: Int =
            querySeqs.map { _.split(':')(2).toInt }.sum

          Clonotype(
            id              = fields(0),
            count           = fields(1).toInt,
            freqPerc        = fields(2).toDouble,
            minSimPercVgene = fields(3).toDouble,
            maxSimPercVgene = fields(4).toDouble,
            avgSimPercVgene = fields(5).toDouble,
            querySeqs       = querySeqs,
            readCount       = readCount
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
}
