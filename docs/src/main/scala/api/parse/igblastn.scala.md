
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

  case object linePrefixes {

    val query                 = "# Query:"
    val domainClassification  = "# Domain classification requested:"
  }

  // TODO will be somewhere else
  case object records {
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

    case object Vgene       extends OutputField[String]
    case object Jgene       extends OutputField[String]
    case object chainType   extends OutputField[ChainTypes]
```

whether the *rearranged?* gene contains a stop codon

```scala
    case object stopCodon   extends OutputField[Boolean]
```

whether the *rearranged?* gene is in-frame

```scala
    case object frame       extends OutputField[Boolean]
```

same as !(stopCodon) && frame

```scala
    case object productive  extends OutputField[Boolean]
    case object strand      extends OutputField[String]

    case object VDJ_annotation extends BlastOutputRecord(
      Vgene       :×:
      Jgene       :×:
      chainType   :×:
      stopCodon   :×:
      frame       :×:
      productive  :×:
      strand      :×:
        |[AnyOutputField]
    )
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
    case object V_end       extends OutputField[String]
```

*Maybe* nucleotides can be inside parentheses here.

```scala
    case object VJ_junction extends OutputField[String]
    case object J_start     extends OutputField[String]
```

I *think* this field will only be there for IG analysis. Nucleotides can be inside parentheses here.

```scala
    case object DJ_junction extends OutputField[String]

    case object VDJ_junction extends BlastOutputRecord(
      V_end       :×:
      VJ_junction :×:
      J_start     :×:
        |[AnyOutputField]
    )
```


### CDR3 annotation

This section is *optional*. Sample output:

```
# Sub-region sequence details (nucleotide sequence, translation)
CDR3	GCAATGGAGGTGGATAGCAGCTATAAATTGATC	AMEVDSSYKLI
```


```scala
    case object CDR3_nucleotides  extends OutputField[String]
    case object CDR3_aminoacids   extends OutputField[String]

    case object CDR3_annotation extends BlastOutputRecord(
      CDR3_nucleotides  :×:
      CDR3_aminoacids   :×:
        |[AnyOutputField]
    )
```


### Hit table

This is equivalent to sthe tandard `blastn` output. The format is

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
    case object segmentType extends OutputField[ChainTypes]

    case object hitTable extends BlastOutputRecord(
      segmentType :×:
      qseqid      :×:
      sseqid      :×:
      pident      :×:
      length      :×:
      mismatch    :×:
      gapopen     :×:
      gaps        :×:
      qstart      :×:
      qend        :×:
      sstart      :×:
      send        :×:
      evalue      :×:
      bitscore    :×:
        |[AnyOutputField]
    )
  }
}

```




[test/scala/CommandGeneration.scala]: ../../../../test/scala/CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: ../../../../test/scala/OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: ../../../../test/scala/OutputFieldsSpecification.scala.md
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