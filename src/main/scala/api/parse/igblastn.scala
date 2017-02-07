package ohnosequences.blast.api.parse

import ohnosequences.blast.api.{BlastOutputRecord, AnyOutputField, OutputField}
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

  The essential bit here is the query ID: the line which starts with `# Query: ` .

  ## Annotations

  ### CDR3

  The format is:

  ```
  # Sub-region sequence details (nucleotide sequence, translation)
  CDR3	GCAATGAGAGGGCCTAACAATGCCAGACTCATG	AMRGPNNARLM
  ```

  thus three fields separated by tabs. I'm *guessing* this could have several lines in the case of IG annotations (??)
*/
case object igblastn {

  case object linePrefixes {

    val query                 = "# Query:"
    val domainClassification  = "# Domain classification requested:"
  }

  // TODO will be somewhere else
  case object records {

    sealed trait Regions
    case object CDR3 extends Regions

    case object region              extends Type[Regions]("region")
    case object nucleotideSequence  extends Type[String]("nucleotide sequence")
    case object translation         extends Type[String]("translation")


    /*
      ### Hit table

      This is equivalent to sthe tandard `blastn` output. The format is

      ```
      # Hit table (the first field indicates the chain type of the hit)
      # Fields: query id, subject id, % identity, alignment length, mismatches, gap opens, gaps, q. start, q. end, s. start, s. end, evalue, bit score
      # 115 hits found
      ```

      followed by record values separated by **tabs**. Note that the order is critical here.
    */
    sealed trait ChainTypes
    case object ChainTypes {
      case object V extends ChainTypes
      case object D extends ChainTypes
      case object J extends ChainTypes
    }
    case object chainType extends OutputField[ChainTypes]

    case object hitTable extends BlastOutputRecord(
      chainType :×:
      qseqid    :×:
      sseqid    :×:
      pident    :×:
      length    :×:
      mismatch  :×:
      gapopen   :×:
      gaps      :×:
      qstart    :×:
      qend      :×:
      sstart    :×:
      send      :×:
      evalue    :×:
      bitscore  :×:
        |[AnyOutputField]
    )
  }

}
