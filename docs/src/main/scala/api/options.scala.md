
```scala
package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, fns._, klists._, typeUnions._
import java.io._

sealed trait AnyBlastOption extends AnyType {
```

The `label` is used for generating the command-line `String` representation of this option. For a BLAST option `-x_yz_abc` name your option here `case object x_yz_abc`.

```scala
  lazy val label: String = s"-${toString}"
```

this is used for serializing values to command-line args

```scala
  val valueToString: Raw => Seq[String]
}

abstract class BlastOption[V](val v: V => String) extends AnyBlastOption {

  type Raw = V

  val valueToString = { x: V => Seq(v(x)) }
}

case object optionValueToSeq extends DepFn1[AnyDenotation, Seq[String]] {

  implicit def default[FO <: AnyBlastOption, V <: FO#Raw](implicit
    option: FO with AnyBlastOption { type Raw = FO#Raw }
  )
  : AnyApp1At[optionValueToSeq.type, FO := V] { type Y = Seq[String] }=
    App1 { v: FO := V => Seq(option.label) ++ option.valueToString(v.value).filterNot(_.isEmpty) }

    implicit def forFlags[FO <: AnyBlastOption { type Raw = Boolean }](implicit
      option: FO
    )
    : AnyApp1At[optionValueToSeq.type, FO := Boolean] { type Y = Seq[String] }=
      App1 {
        v: FO := Boolean =>
          if(v.value)
            Seq(option.label) ++ option.valueToString(v.value).filterNot(_.isEmpty)
          else
            Seq()
      }
}
```

This works as a type class, which provides a way of serializing a list of AnyBlastOption's

```scala
trait BlastOptionsToSeq[L <: AnyKList.withBound[AnyDenotation]] {

  def apply(l: L): Seq[String]
}

case object BlastOptionsToSeq {

  implicit def default[L <: AnyKList.withBound[AnyDenotation], O <: AnyKList.withBound[Seq[String]]](implicit
    mapp: AnyApp2At[mapKList[optionValueToSeq.type, Seq[String]], optionValueToSeq.type, L] { type Y = O }
  ): BlastOptionsToSeq[L] = new BlastOptionsToSeq[L] {
      def apply(l: L): Seq[String] = mapp(optionValueToSeq, l).asList.flatten
  }
}
```


### Options

As the same options are valid for several commands, they are defined independently here.


```scala
case object db    extends BlastOption[Set[File]](f => f.toList.map(_.getCanonicalPath).mkString(" "))
case object query extends BlastOption[File](f => f.getCanonicalPath)
case object out   extends BlastOption[File](f => f.getCanonicalPath)

case object num_threads     extends BlastOption[Int](n => n.toString)
case object evalue          extends BlastOption[BigDecimal](n => n.toString)
case object max_target_seqs extends BlastOption[Int](n => n.toString)
case object show_gis        extends BlastOption[Boolean](t => "")

case object word_size extends BlastOption[Int](n => if(n < 4) 4.toString else n.toString)
```

penalty needs to be ≤ 0

```scala
case object penalty extends BlastOption[Int](n => if(n > 0) 0.toString else n.toString)
```

reward needs to be ≥ 0

```scala
case object reward extends BlastOption[Int](n => if(n < 0) 0.toString else n.toString)
case object ungapped extends BlastOption[Boolean](t => "")

case object strand extends BlastOption[Strands](_.toString)

sealed trait Strands
case object Strands {

  case object both  extends Strands
  case object minus extends Strands
  case object plus  extends Strands
}

// TODO the default values website says that this is an int?!
case object perc_identity extends BlastOption[Double](n => if(n > 100 || n < 0) 0.toString else n.toString)
```


#### IgBLAST options

Right now we have as a comment the relevant output of `igblastn -help`


```scala
// -germline_db_V <String>
//    Germline database name
case object germline_db_V extends BlastOption[File](_.getCanonicalPath)

//  -num_alignments_V <Integer>
//    Number of Germline sequences to show alignments for
//    Default = `3'
case object num_alignments_V extends BlastOption[Int](_.toString)

//  -germline_db_V_seqidlist <String>
//    Restrict search of germline database to list of SeqIds's

//  -germline_db_D <String>
//    Germline database name
case object germline_db_D extends BlastOption[File](_.getCanonicalPath)

//  -num_alignments_D <Integer>
//    Number of Germline sequences to show alignments for
//    Default = `3'
case object num_alignments_D extends BlastOption[Int](_.toString)

//  -germline_db_D_seqidlist <String>
//    Restrict search of germline database to list of SeqIds's

//  -germline_db_J <String>
//    Germline database name
case object germline_db_J extends BlastOption[File](_.getCanonicalPath)

//  -num_alignments_J <Integer>
//    Number of Germline sequences to show alignments for
//    Default = `3'
case object num_alignments_J extends BlastOption[Int](_.toString)

//  -germline_db_J_seqidlist <String>
//    Restrict search of germline database to list of SeqIds's

//  -auxiliary_data <String>
//    File containing the coding frame start positions for sequences in germline
//    J database
case object auxiliary_data extends BlastOption[File](_.getCanonicalPath)

//  -min_D_match <Integer, >=5>
//    Required minimal number of D gene matches
case object min_D_match extends BlastOption[Int](n => ( if(n < 5) 5 else n ).toString)

//  -D_penalty <Integer, (> -6 and <0)>
//    Penalty for a nucleotide mismatch in D gene
//    Default = `-4'
case object D_penalty extends BlastOption[Int](n => ( if(n > -6 && n < 0) n else -4 ).toString)

//  -num_clonotype <Integer, >=0>
//    Number of top clonotypes to show
//    Default = `100'
case object num_clonotype extends BlastOption[Int](n => (if(n < 0) 100 else n).toString)

//  -clonotype_out <File_Out>
//    Output file name for clonotype info
case object clonotype_out extends BlastOption[File](_.getCanonicalPath)

//  -organism <String, `human', `mouse', `rabbit', `rat', `rhesus_monkey'>
//    The organism for your query sequence (i.e., human, mouse, etc.)
//    Default = `human'
case object organism extends BlastOption[IgBLASTOrganism](_.toString)
sealed trait IgBLASTOrganism
case object IgBLASTOrganism {
  case object human         extends IgBLASTOrganism
  case object mouse         extends IgBLASTOrganism
  case object rabbit        extends IgBLASTOrganism
  case object rat           extends IgBLASTOrganism
  case object rhesus_monkey extends IgBLASTOrganism
}

//  -domain_system <String, `imgt', `kabat'>
//    Domain system to be used for segment annotation
//    Default = `imgt'
case object domain_system extends BlastOption[IgBLASTDomainSystem](_.toString)
sealed trait IgBLASTDomainSystem
case object IgBLASTDomainSystem {
  case object imgt  extends IgBLASTDomainSystem
  case object kabat extends IgBLASTDomainSystem
}

//  -ig_seqtype <String, `Ig', `TCR'>
case object ig_seqtype extends BlastOption[IgBLASTSeqType](_.toString)
sealed trait IgBLASTSeqType
case object IgBLASTSeqType {
  case object Ig  extends IgBLASTSeqType
  case object TCR extends IgBLASTSeqType
}

// -focus_on_V_segment
//    Should the search only be for V segment (effective only for non-germline
//    database search using -db option)?
case object focus_on_V_segment extends BlastOption[Boolean](_=>"")

//  -extend_align5end
//    Extend V gene alignment at 5' end
case object extend_align5end extends BlastOption[Boolean](_=>"")

//  -min_V_length <Integer, >=9>
//    Minimal required V gene length
//    Default = `9'
case object min_V_length extends BlastOption[Int](n => (if(n < 9) 9 else n).toString)

//  -min_J_length <Integer, >=0>
//    Minimal required J gene length
//    Default = `0'
case object min_J_length extends BlastOption[Int](n => (if(n < 0) 0 else n).toString)

//  -show_translation
//    Show translated alignments
case object show_translation extends BlastOption[Boolean](_=>"")
```


#### `makeblastdb`-specific options


```scala
case object title extends BlastOption[String](x => x)
case object in extends BlastOption[File](f => f.getCanonicalPath)


case object input_type extends BlastOption[DBInputType](t => t.toString)

sealed trait DBInputType
case object DBInputType {

  case object asn1_bin  extends DBInputType
  case object asn1_txt  extends DBInputType
  case object blastdb   extends DBInputType
  case object fasta     extends DBInputType
}

case object dbtype extends BlastOption[BlastDBType](t => t.toString)

sealed trait BlastDBType
case object BlastDBType {
  case object nucl extends BlastDBType
  case object prot extends BlastDBType
}

case object parse_seqids extends BlastOption[Boolean](_ => "")

```




[test/scala/CommandGeneration.scala]: ../../../test/scala/CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: ../../../test/scala/OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: ../../../test/scala/OutputFieldsSpecification.scala.md
[main/scala/api/outputFields.scala]: outputFields.scala.md
[main/scala/api/options.scala]: options.scala.md
[main/scala/api/package.scala]: package.scala.md
[main/scala/api/expressions.scala]: expressions.scala.md
[main/scala/api/commands/blastn.scala]: commands/blastn.scala.md
[main/scala/api/commands/blastp.scala]: commands/blastp.scala.md
[main/scala/api/commands/tblastx.scala]: commands/tblastx.scala.md
[main/scala/api/commands/tblastn.scala]: commands/tblastn.scala.md
[main/scala/api/commands/blastx.scala]: commands/blastx.scala.md
[main/scala/api/commands/makeblastdb.scala]: commands/makeblastdb.scala.md
[main/scala/api/commands/igblastn.scala]: commands/igblastn.scala.md