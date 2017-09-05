
```scala
package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, klists._
import ohnosequences.blast.api.outputFields._

case object igblastn extends AnyBlastCommand {

  type Arguments = arguments.type
  case object arguments extends RecordType(
    query         :×:
    germline_db_V :×:
    germline_db_D :×:
    germline_db_J :×:
    ig_seqtype    :×:
    organism      :×:
    clonotype_out :×:
    out           :×:
    |[AnyBlastOption]
  )

  type Options = options.type
  case object options extends RecordType(
    num_threads         :×:
    evalue              :×:
    max_target_seqs     :×:
    strand              :×:
    word_size           :×:
    show_gis            :×:
    ungapped            :×:
    penalty             :×:
    reward              :×:
    perc_identity       :×:
    // IgBLAST-specific
    num_alignments_V    :×:
    num_alignments_D    :×:
    num_alignments_J    :×:
    min_V_length        :×:
    min_J_length        :×:
    min_D_match         :×:
    D_penalty           :×:
    extend_align5end    :×:
    num_clonotype       :×:
    show_translation    :×:
    domain_system       :×:
    focus_on_V_segment  :×:
    |[AnyBlastOption]
  )

  type ArgumentsVals =
    (query.type := query.Raw)                  ::
    (germline_db_V.type := germline_db_V.Raw)  ::
    (germline_db_D.type := germline_db_D.Raw)  ::
    (germline_db_J.type := germline_db_J.Raw)  ::
    (ig_seqtype.type := ig_seqtype.Raw)        ::
    (organism.type := organism.Raw)            ::
    (clonotype_out.type := clonotype_out.Raw)  ::
    (out.type := out.Raw)                      ::
    *[AnyDenotation]

  type OptionsVals =
    (num_threads.type         := num_threads.Raw)         ::
    (evalue.type              := evalue.Raw)              ::
    (max_target_seqs.type     := max_target_seqs.Raw)     ::
    (strand.type              := strand.Raw)              ::
    (word_size.type           := word_size.Raw)           ::
    (show_gis.type            := show_gis.Raw)            ::
    (ungapped.type            := ungapped.Raw)            ::
    (penalty.type             := penalty.Raw)             ::
    (reward.type              := reward.Raw)              ::
    (perc_identity.type       := perc_identity.Raw)       ::
    (num_alignments_V.type    := num_alignments_V.Raw)    ::
    (num_alignments_D.type    := num_alignments_D.Raw)    ::
    (num_alignments_J.type    := num_alignments_J.Raw)    ::
    (min_V_length.type        := min_V_length.Raw)        ::
    (min_J_length.type        := min_J_length.Raw)        ::
    (min_D_match.type         := min_D_match.Raw)         ::
    (D_penalty.type           := D_penalty.Raw)           ::
    (extend_align5end.type    := extend_align5end.Raw)    ::
    (num_clonotype.type       := num_clonotype.Raw)       ::
    (show_translation.type    := show_translation.Raw)    ::
    (domain_system.type       := domain_system.Raw)       ::
    (focus_on_V_segment.type  := focus_on_V_segment.Raw)  ::
    *[AnyDenotation]
```

Default values match those documented in [the official BLAST docs](http://www.ncbi.nlm.nih.gov/books/NBK279675/) whenever possible.

```scala
  val defaults: Options := OptionsVals = options (
    num_threads(1)                          ::
    evalue(BigDecimal(10))                  ::
    max_target_seqs(500)                    ::
    strand(Strands.both)                    ::
    word_size(11)                           ::
    show_gis(false)                         ::
    ungapped(false)                         ::
    penalty(-3)                             ::
    reward(2)                               ::
    perc_identity(0D)                       ::
    num_alignments_V(3)                     ::
    num_alignments_D(3)                     ::
    num_alignments_J(3)                     ::
    min_V_length(9)                         ::
    min_J_length(0)                         ::
    min_D_match(5)                          ::
    D_penalty(-4)                           ::
    extend_align5end(false)                 ::
    num_clonotype(100)                      ::
    show_translation(false)                 ::
    domain_system(IgBLASTDomainSystem.imgt) ::
    focus_on_V_segment(false)               ::
    *[AnyDenotation]
  )
```

`igblastn` does not support the csv output format

```scala
  type ValidOutputFields =
    |[AnyOutputField]

  case class Expression(argumentValues: ArgumentsVals, optionValues: OptionsVals) {

    lazy val argValsToSeq: BlastOptionsToSeq[ArgumentsVals] =
      implicitly[BlastOptionsToSeq[ArgumentsVals]]

    lazy val optValsToSeq: BlastOptionsToSeq[OptionsVals] =
      implicitly[BlastOptionsToSeq[OptionsVals]]

    def toSeq: Seq[String] =
      igblastn.name                 ++
      argValsToSeq(argumentValues)  ++
      optValsToSeq(optionValues)    ++
      Seq("-outfmt", "7")
  }

  def apply(argumentValues: ArgumentsVals, optionValues: OptionsVals): Expression =
    Expression(argumentValues, optionValues)
```


## igblastn output

We have different outputs depending on the type of query sequences.


```scala
  case object output {

    case object TCRA {

      // possibly not all of these records will be truly specific; we'll see later
      case object VJRearrangementSummary extends BlastOutputRecord(
        topVGene    :×:
        topJGene    :×:
        chainType   :×:
        stopCodon   :×:
        frame       :×:
        productive  :×:
        strand      :×:
          |[AnyOutputField]
      )

      case object VJJunctionDetails extends BlastOutputRecord(
        VEnd       :×:
        VJJunction :×:
        JStart     :×:
          |[AnyOutputField]
      )

      case object CDR3Sequence        extends BlastOutputRecord(stdCDR3SeqFields)
      case object VRegionAnnotations  extends BlastOutputRecord(stdVRegionAnnotationFields)
      case object HitTable            extends BlastOutputRecord(stdHitTableFields)
    }

    case object TCRB {

      case object VDJRearrangementSummary extends BlastOutputRecord(
        topVGene    :×:
        topDGene    :×:
        topJGene    :×:
        chainType   :×:
        stopCodon   :×:
        frame       :×:
        productive  :×:
        strand      :×:
          |[AnyOutputField]
      )

      case object VDJunctionDetails extends BlastOutputRecord(
        VEnd       :×:
        VDJunction :×:
        DRegion    :×:
        DJJunction :×:
        JStart     :×:
          |[AnyOutputField]
      )

      case object CDR3Sequence        extends BlastOutputRecord(stdCDR3SeqFields)
      case object VRegionAnnotations  extends BlastOutputRecord(stdVRegionAnnotationFields)
      case object HitTable            extends BlastOutputRecord(stdHitTableFields)
    }

    val stdCDR3SeqFields =
      CDR3Nucleotides  :×:
      CDR3Aminoacids   :×:
        |[AnyOutputField]

    // hit tables are common to all outputs (are they?)
    val stdHitTableFields =
      segmentType         :×:
      qseqid              :×:
      sseqid              :×:
      pident              :×:
      length              :×:
      mismatch            :×:
      gapopen             :×:
      gaps                :×:
      qstart              :×:
      qend                :×:
      sstart              :×:
      send                :×:
      outputFields.evalue :×:
      bitscore            :×:
        |[AnyOutputField]

    val stdVRegionAnnotationFields =
      VRegion   :×:
      sstart    :×:
      send      :×:
      length    :×:
      nident    :×: // TODO should it be `positive` instead?
      mismatch  :×:
      gaps      :×:
      pident    :×:
        |[AnyOutputField]
```


### VDJ annotation summary

If there are any hits, this section will be present too (I think). Sample output:

```
# V-(D)-J rearrangement summary for query sequence (Top V gene match, Top J gene match, Chain type, stop codon, V-J frame, Productive, Strand).  Multiple equivalent top matches, if present, are separated by a comma.
TRAV14/DV4*02	TRAJ31*01	VA	No	In-frame	Yes	+
```


```scala
    sealed trait ChainTypes
    // TODO review these instances
    case object ChainTypes {
      case object VA extends ChainTypes
      case object VB extends ChainTypes
      case object VD extends ChainTypes
      case object VG extends ChainTypes
      case object VH extends ChainTypes
    }

    implicit val chainTypeParser: DenotationParser[chainType.type,ChainTypes,String] =
      new DenotationParser(chainType, chainType.label)(
        {
          str: String => str match {
            case "VA" => Some(ChainTypes.VA)
            case "VB" => Some(ChainTypes.VB)
            case "VD" => Some(ChainTypes.VD)
            case "VG" => Some(ChainTypes.VG)
            case "VH" => Some(ChainTypes.VH)
            case _    => None
          }
        }
      )

    case object chainType   extends OutputField[ChainTypes]

    // TODO these top* fields are actually a set, delimited by commas.
    case object topVGene       extends OutputField[String]
    implicit val topVGeneParser: DenotationParser[topVGene.type, String, String] =
      new DenotationParser(topVGene, topVGene.label)({ Some(_) })

    case object topDGene       extends OutputField[String]
    implicit val topDGeneParser: DenotationParser[topDGene.type, String, String] =
      new DenotationParser(topDGene, topDGene.label)({ Some(_) })

    case object topJGene       extends OutputField[String]
    implicit val topJGeneParser: DenotationParser[topJGene.type, String, String] =
      new DenotationParser(topJGene, topJGene.label)({ Some(_) })
```

whether the *rearranged?* gene contains a stop codon

```scala
    case object stopCodon   extends OutputField[Boolean]
    implicit val stopCodonParser: DenotationParser[stopCodon.type, Boolean, String] =
      new DenotationParser(stopCodon, stopCodon.label)(
        {
          s: String => s.trim match {
            case "Yes"  => Some(true)
            case "No"   => Some(false)
            case _      => None
          }
        }
      )
```

whether the *rearranged?* gene is in-frame

```scala
    case object frame       extends OutputField[Boolean]
    implicit val frameParser: DenotationParser[frame.type, Boolean, String] =
      new DenotationParser(frame, frame.label)(
        {
          s: String => s match {
            case "In-frame" => Some(true)
            case "???"      => Some(false) // TODO get String constant for this case
            case _          => None
          }
        }
      )
```

same as !(stopCodon) && frame

```scala
    case object productive  extends OutputField[Boolean]
    implicit val productiveParser: DenotationParser[productive.type, Boolean, String] =
      new DenotationParser(productive, productive.label)(
        {
          s: String => s match {
            case "Yes"  => Some(true)
            case "No"   => Some(false)
            case _      => None
          }
        }
      )

    case object strand      extends OutputField[String]
    implicit val strandParser: DenotationParser[strand.type, String, String] =
      new DenotationParser(strand, strand.label)({ Some(_) })
```


### VDJ junction

This section is *optional*. Sample output for TCR α:

```
# V-(D)-J junction details based on top germline gene matches (V end, V-J junction, J start).  Note that possible overlapping nucleotides at VDJ junction (i.e, nucleotides that could be assigned to either rearranging gene) are indicated in parentheses (i.e., (TACT)) but are not included under the V, D, or J gene itself
GAGAG	CC	CCAGG
```

and for IGH:

```
# V-(D)-J junction details based on top germline gene matches (V end, V-D junction, D region, D-J junction, J start).  Note that possible overlapping nucleotides at VDJ junction (i.e, nucleotides that could be assigned to either rearranging gene) are indicated in parentheses (i.e., (TACT)) but are not included under the V, D, or J gene itself
AGAGG	CAGTACCGGC	CGATTTTTGGAGTGGTTATTA	(TAC)	TTTGA
```

Quoting from the paper:

> It is worth noting that the IgBLAST report provides information on overlapping nucleotides at a rearrangement junction that might have been contributed by either of the rearranging genes because of homology directed recombination events (13). Such nucleotides are listed inside a parenthesis under the relevant junction in the summary table

I think that for IG you have a record with the four fields below, while for TCR

1. α has V-J-C, so it does *not* have `D` and so no `DJ_junction`
2. β has  V-D-J-C so it has (in principle) `DJ_junction`

We can

1. have different commands for each type of analysis (prefered)
2. have one record with an Option value (ugly)
3. something else (what?)


```scala
    case object VEnd       extends OutputField[String]
    implicit val VEndParser: DenotationParser[VEnd.type, String, String] =
      new DenotationParser(VEnd, VEnd.label)({ Some(_) })
```

nucleotides can be inside parentheses here.

```scala
    case object VDJunction extends OutputField[String]
    implicit val VDJunctionParser: DenotationParser[VDJunction.type, String, String] =
      new DenotationParser(VDJunction, VDJunction.label)({ Some(_) })
```

nucleotides can be inside parentheses here.

```scala
    case object VJJunction extends OutputField[String]
    implicit val VJJunctionParser: DenotationParser[VJJunction.type, String, String] =
      new DenotationParser(VJJunction, VJJunction.label)({ Some(_) })

    case object DRegion    extends OutputField[String]
    implicit val DRegionParser: DenotationParser[DRegion.type, String, String] =
      new DenotationParser(DRegion, DRegion.label)({ Some(_) })
```

nucleotides can be inside parentheses here.

```scala
    case object DJJunction extends OutputField[String]
    implicit val DJJunctionParser: DenotationParser[DJJunction.type, String, String] =
      new DenotationParser(DJJunction, DJJunction.label)({ Some(_) })

    case object JStart      extends OutputField[String]
    implicit val JStartParser: DenotationParser[JStart.type, String, String] =
      new DenotationParser(JStart, JStart.label)({ Some(_) })
```


### CDRx annotation

This table contains the CDR[1-3] annotations; each one is a record with standard BLAST output values. Sample output:

```
# Alignment summary between query and top germline V gene hit (from, to, length, matches, mismatches, gaps, percent identity)
FR1-IMGT	59	136	78	78	0	0	100
CDR1-IMGT	137	156	21	20	0	1	95.2
FR2-IMGT	157	205	51	47	2	2	92.2
CDR2-IMGT	206	228	24	23	0	1	95.8
FR3-IMGT	229	331	104	101	0	3	97.1
CDR3-IMGT (germline)	332	339	8	8	0	0	100
Total	N/A	N/A	286	277	2	7	96.9
```


```scala
    sealed trait VRegions
    case object VRegions {
      case object FR1   extends VRegions
      case object CDR1  extends VRegions
      case object FR2   extends VRegions
      case object CDR2  extends VRegions
      case object FR3   extends VRegions
      case object CDR3  extends VRegions
    }

    case object VRegion extends OutputField[VRegions]
    implicit val VRegionParser: DenotationParser[VRegion.type,VRegions,String] =
      new DenotationParser(VRegion, VRegion.label)(
        {
          str: String => str match {
            case "FR1-IMGT"             => Some(VRegions.FR1)
            case "CDR1-IMGT"            => Some(VRegions.CDR1)
            case "FR2-IMGT"             => Some(VRegions.FR2)
            case "CDR2-IMGT"            => Some(VRegions.CDR2)
            case "FR3-IMGT"             => Some(VRegions.FR3)
            case "CDR3-IMGT (germline)" => Some(VRegions.CDR3)
            case _                      => None
          }
        }
      )
```


#### CDR3 annotation

Note that this section is *optional*. Sample output:

```
# Sub-region sequence details (nucleotide sequence, translation)
CDR3	GCAATGGAGGTGGATAGCAGCTATAAATTGATC	AMEVDSSYKLI
```


```scala
    case object CDR3Nucleotides  extends OutputField[String]
    implicit val CDR3NucleotidesParser: DenotationParser[CDR3Nucleotides.type, String, String] =
      new DenotationParser(CDR3Nucleotides, CDR3Nucleotides.label)({ Some(_) })

    case object CDR3Aminoacids   extends OutputField[String]
    implicit val CDR3AminoacidsParser: DenotationParser[CDR3Aminoacids.type, String, String] =
      new DenotationParser(CDR3Aminoacids, CDR3Aminoacids.label)({ Some(_) })
```


### Hit table

This is equivalent to the tandard `blastn` output. The format is

```
# Hit table (the first field indicates the chain type of the hit)
# Fields: query id, subject id, % identity, alignment length, mismatches, gap opens, gaps, q. start, q. end, s. start, s. end, evalue, bit score
# 115 hits found
V	AY671579.1	IGHV4-34*01	97.952	293	6	0	0	3	295	1	293	3.00e-125	439
V	AY671579.1	IGHV4-34*02	97.611	293	7	0	0	3	295	1	293	2.60e-124	436
V	AY671579.1	IGHV4-34*04	97.270	293	8	0	0	3	295	1	293	2.26e-123	433
D	AY671579.1	IGHD3-3*01	100.000	24	0	0	0	306	329	7	30	8.55e-10	48.3
D	AY671579.1	IGHD3-3*02	100.000	22	0	0	0	308	329	9	30	1.36e-08	44.3
D	AY671579.1	IGHD3-22*01	100.000	11	0	0	0	316	326	17	27	0.055	22.4
J	AY671579.1	IGHJ4*02	100.000	46	0	0	0	327	372	3	48	1.56e-22	91.7
J	AY671579.1	IGHJ4*01	97.826	46	1	0	0	327	372	3	48	3.81e-20	83.8
J	AY671579.1	IGHJ4*03	95.652	46	2	0	0	327	372	3	48	9.28e-18	75.8
```

Record values separated by **tabs**. Note that the column order is critical here.


```scala
    sealed trait SegmentTypes
    case object SegmentTypes {
      case object V extends SegmentTypes
      case object D extends SegmentTypes
      case object J extends SegmentTypes
    }
    case object segmentType extends OutputField[SegmentTypes]
    implicit val segmentTypeParser: DenotationParser[segmentType.type,SegmentTypes,String] =
      new DenotationParser(segmentType, segmentType.label)(
        {
          str: String => str match {
            case "V"  => Some(SegmentTypes.V)
            case "D"  => Some(SegmentTypes.D)
            case "J"  => Some(SegmentTypes.J)
            case _    => None
          }
        }
      )
  }
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
[main/scala/api/parse/igblastn.scala]: ../parse/igblastn.scala.md
[main/scala/api/commands/blastn.scala]: blastn.scala.md
[main/scala/api/commands/blastp.scala]: blastp.scala.md
[main/scala/api/commands/tblastx.scala]: tblastx.scala.md
[main/scala/api/commands/tblastn.scala]: tblastn.scala.md
[main/scala/api/commands/blastx.scala]: blastx.scala.md
[main/scala/api/commands/makeblastdb.scala]: makeblastdb.scala.md
[main/scala/api/commands/igblastn.scala]: igblastn.scala.md