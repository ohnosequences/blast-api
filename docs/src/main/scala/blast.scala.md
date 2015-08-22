
```scala
package ohnosequences.blast

case object api {

  import ohnosequences.cosas._, typeSets._, properties._, types._, records._
  import ohnosequences.cosas.ops.typeSets.{ CheckForAll, ToList, MapToList }
  import shapeless.poly._
  import java.io.File
```


This trait models a command part of the `BLAST` suite, like `blastn`, `blastp`, or `makeblastdb`. It is a property, with values of that property being valid command expressions.


```scala
  sealed trait AnyBlastCommand extends AnyProperty {
```

This label should match the name of the command

```scala
    lazy val label: String = toString

    type Arguments  <: AnyRecord { type PropertySet <: AnyPropertySet.withBound[AnyBlastOption] }
    type Options    <: AnyRecord { type PropertySet <: AnyPropertySet.withBound[AnyBlastOption] }
```

default values for options; they are *optional*, so should have default values.

```scala
    val defaults: ValueOf[Options]
    val defaultsAsSeq: Seq[String]

    type Raw >: (ValueOf[Arguments], ValueOf[Options]) <: (ValueOf[Arguments], ValueOf[Options])
  }

  sealed trait AnyBlastOption extends AnyProperty {
```

The `label` is used for generating the command-line `String` representation of this option. For a BLAST option `-x_yz_abc` name your option here `case object x_yz_abc`.

```scala
    lazy val label: String = s"-${toString}"
```

this is used for serializing values to command-line args

```scala
    val valueToString: Raw => String
  }
  case object AnyBlastOption {

    type is[B <: AnyBlastOption] = B with AnyBlastOption { type Raw = B#Raw }
  }
  abstract class BlastOption[V](val valueToString: V => String) extends AnyBlastOption { type Raw = V }
```


### `Seq[String]` Command generation

for command values we generate a `Seq[String]` which is valid command expression that you can execute (assuming BLAST installed) using `scala.sys.process` or anything similar.


```scala
  case object optionValueToSeq extends shapeless.Poly1 {

    implicit def default[BO <: AnyBlastOption](implicit option: AnyBlastOption.is[BO]) =
      at[ValueOf[BO]]{ v: ValueOf[BO] => (Seq[String]( option.label, option.valueToString(v.value) )).filterNot(_.isEmpty) }
  }

  implicit def blastCommandValueOps[B <: AnyBlastCommand](cmdValueOf: ValueOf[B]): BlastCommandValueOps[B] =
    BlastCommandValueOps(cmdValueOf.value)
  case class BlastCommandValueOps[B <: AnyBlastCommand](val cmdValue: B#Raw) extends AnyVal {

    def cmd(implicit
      cmd: B,
      mapArgs: (optionValueToSeq.type MapToList B#Arguments#Raw) { type O = Seq[String] },
      mapOpts: (optionValueToSeq.type MapToList B#Options#Raw) { type O = Seq[String] }
    ): Seq[String] = {

      val (argsSeqs, optsSeqs): (List[Seq[String]], List[Seq[String]]) = (
        (cmdValue._1.value: B#Arguments#Raw) mapToList optionValueToSeq,
        (cmdValue._2.value: B#Options#Raw) mapToList optionValueToSeq
      )

      Seq(cmd.label) ++ argsSeqs.toSeq.flatten ++ optsSeqs.toSeq.flatten
    }
  }
```


### BLAST command instances

All the BLAST suite commands, together with their arguments, options and default values.


```scala
  type blastn = blastn.type
  case object blastn extends AnyBlastCommand {

    type Arguments = arguments.type
    case object arguments extends Record(db :&: query :&: out :&: □)
    type Options = options.type
    case object options extends Record(
      num_threads :&:
      task        :&:
      evalue      :&:
      strand      :&:
      word_size   :&:
      show_gis    :&:
      ungapped    :&: □
    )

    val defaults = options(
      num_threads(1)                :~:
      task(blastn)                  :~:
      evalue(10)                    :~:
      strand(Strands.both)          :~:
      word_size(4)                  :~:
      show_gis(false)               :~:
      ungapped(false)               :~: ∅
    )

    lazy val defaultsAsSeq = (defaults.value mapToList optionValueToSeq).flatten

    // task depends on each command, that's why it is here.
    case object task extends BlastOption[Task](t => t.name)
    sealed abstract class Task(val name: String)
    case object megablast       extends Task( "megablast" )
    case object dcMegablast     extends Task( "dc-megablast" )
    case object blastn          extends Task( "blastn" )
    case object blastnShort     extends Task( "blastn-short" )
    case object rmblastn        extends Task( "rmblastn" )
  }

  type blastp = blastp.type
  case object blastp extends AnyBlastCommand {

    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
    case object options extends Record(num_threads :&: □)
    type Options = options.type

    val defaults = options(
      num_threads(1) :~: ∅
    )

    lazy val defaultsAsSeq = (defaults.value mapToList optionValueToSeq).flatten

    case object task extends BlastOption[Task](t => t.name)
    sealed abstract class Task(val name: String)
    case object blastp      extends Task( "blastp" )
    case object blastpFast  extends Task( "blastp-fast" )
    case object blastpShort extends Task( "blastp-short" )
  }

  type blastx   = blastx.type
  case object blastx extends AnyBlastCommand {

    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
    case object options extends Record(num_threads :&: □)
    type Options = options.type

    val defaults = options(
      num_threads(1) :~: ∅
    )

    lazy val defaultsAsSeq = (defaults.value mapToList optionValueToSeq).flatten

    case object task extends BlastOption[Task](t => t.name)
    sealed abstract class Task(val name: String)
    case object blastx     extends Task( "blastx" )
    case object blastxFast extends Task( "blastx-fast" )
  }

  type tblastn = tblastn.type
  case object tblastn extends AnyBlastCommand {

    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
    case object options extends Record(num_threads :&: □)
    type Options = options.type

    val defaults = options := num_threads(1) :~: ∅

    lazy val defaultsAsSeq = (defaults.value mapToList optionValueToSeq).flatten

    case object task extends BlastOption[Task](t => t.name)
    sealed abstract class Task(val name: String)
    case object tblastn     extends Task( "tblastn" )
    case object tblastnFast extends Task( "tblastn-fast" )
  }

  type tblastx = tblastx.type
  case object tblastx extends AnyBlastCommand {

    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
    case object options extends Record(num_threads :&: □)
    type Options = options.type

    val defaults = options := num_threads(1) :~: ∅

    lazy val defaultsAsSeq = (defaults.value mapToList optionValueToSeq).flatten
  }

  type makeblastdb = makeblastdb.type
  case object makeblastdb extends AnyBlastCommand {

    case object arguments extends Record(in :&: input_type :&: dbtype :&: □)
    type Arguments = arguments.type
    case object options extends Record(title :&: □)
    type Options = options.type

    val defaults = options := title("") :~: ∅

    lazy val defaultsAsSeq = (defaults.value mapToList optionValueToSeq).flatten
  }
```


### Options

As the same options are valid for several commands, they are defined independently here.


```scala
  case object db    extends BlastOption[File](f => f.getCanonicalPath.toString)
  case object query extends BlastOption[File](f => f.getCanonicalPath.toString)
  case object out   extends BlastOption[File](f => f.getCanonicalPath.toString)

  case object num_threads     extends BlastOption[Int](n => n.toString)
  case object evalue          extends BlastOption[Double](n => n.toString)
  case object max_target_seqs extends BlastOption[Int](n => n.toString)
  case object show_gis        extends BlastOption[Boolean](t => "")

  case object word_size extends BlastOption[Int](n => if( n < 4 ) 4.toString else n.toString )
  case object ungapped extends BlastOption[Boolean](t => "")

  case object strand extends BlastOption[Strands](_.toString)
  sealed trait Strands
  case object Strands {
    case object both  extends Strands
    case object minus extends Strands
    case object plus  extends Strands
  }
```


#### `makeblastdb`-specific options


```scala
  case object title extends BlastOption[String](x => x)
  case object in extends BlastOption[File](f => f.getCanonicalPath.toString)

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
```


### BLAST output

There is a lot that can be specified for BLAST output. See below for output format types and output fields.


```scala
  // sealed trait AnyOutputFormat extends AnyBlastOption {
  //
  //   // TODO check it
  //   type Commands = AllBlasts
  //   val commands = allBlasts
  //
  //   type OutputFormatType <: AnyOutputFormatType
  //   val outputFormatType: OutputFormatType
  //
  //   type OutputRecordFormat <: AnyTypeSet.Of[AnyOutputField]
  //   val outputRecordFormat: OutputRecordFormat
  //
  //   implicit val outputRecordFormatList: ToListOf[OutputRecordFormat, AnyOutputField]
  //
  //   lazy val outputRecordFormatStr = (outputRecordFormat.toListOf[AnyOutputField] map {_.toString}).mkString(" ")
  //   lazy val code: Int = outputFormatType.code
  //   lazy val toSeq: Seq[String] = Seq("-outfmt", s"'${code} ${outputRecordFormatStr}'")
  // }
  // case class outfmt[
  //   T <: AnyOutputFormatType,
  //   OF <: AnyTypeSet.Of[AnyOutputField]
  // ]
  // (
  //   val outputFormatType: T,
  //   val outputRecordFormat: OF
  // )(implicit
  //   val outputRecordFormatList: ToListOf[OF, AnyOutputField]
  // )
  // extends AnyOutputFormat {
  //
  //   type OutputFormatType = T
  //   type OutputRecordFormat = OF
  // }

```


### BLAST output formats and fields

A lot of different outputs, plus the possibility of choosing fields for CSV/TSV output.


```scala
  sealed trait AnyOutputFormatType { val code: Int }
  abstract class OutputFormatType(val code: Int) extends AnyOutputFormatType
  case object format {

    case object pairwise                  extends OutputFormatType(0)
    case object queryAnchoredShowIds      extends OutputFormatType(1)
    case object queryAnchoredNoIds        extends OutputFormatType(2)
    case object flatQueryAnchoredShowIds  extends OutputFormatType(3)
    case object flatQueryAnchoredNoIds    extends OutputFormatType(4)
    case object XML                       extends OutputFormatType(5)
    case object TSV                       extends OutputFormatType(6)
    case object TSVWithComments           extends OutputFormatType(7)
    case object TextASN1                  extends OutputFormatType(8)
    case object BinaryASN1                extends OutputFormatType(9)
    case object CSV                       extends OutputFormatType(10)
    case object BLASTArchiveASN1          extends OutputFormatType(11)
    case object JSONSeqalign              extends OutputFormatType(12)
  }

  sealed trait AnyOutputField extends AnyProperty {

    type Commands <: AnyTypeSet.Of[AnyBlastCommand]
  }

  trait OutputField[V] extends AnyOutputField {

    type Raw = V
    lazy val label: String = toString
  }

  trait OutputFieldFor[C <: AnyBlastCommand] extends TypePredicate[AnyOutputField] {

    type Condition[Flds <: AnyOutputField] = C isIn Flds#Commands
  }
```

Inside this object you have all the possible fields that you can specify as output

```scala
  case object outFields {
```

Auxiliary type for setting the valid commands for an output field.

```scala
    trait ForCommands[Cmmnds <: AnyTypeSet.Of[AnyBlastCommand]] extends AnyOutputField {

      type Commands = Cmmnds
    }
```

Query Seq-id

```scala
    case object qseqid    extends OutputField[String]
```

Query GI

```scala
    case object qgi       extends OutputField[String]
    // means Query accesion
    case object qacc      extends OutputField[String]
    // means Query accesion.version
    case object qaccver   extends OutputField[Int]
    // means Query sequence length
    case object qlen      extends OutputField[Int]
    // means Subject Seq-id
    case object sseqid    extends OutputField[String]
    // means All subject Seq-id(s), separated by a ';'
    case object sallseqid extends OutputField[List[String]]
    // means Subject GI
    case object sgi       extends OutputField[String]
    // means All subject GIs
    case object sallgi    extends OutputField[List[String]]
    // means Subject accession
    case object sacc      extends OutputField[String]
    // means Subject accession.version
    case object saccver   extends OutputField[String]
    // means All subject accessions
    case object sallacc   extends OutputField[String]
    // means Subject sequence length
    case object slen      extends OutputField[Int]
    // means Start of alignment in query
    case object qstart    extends OutputField[Int]
    // means End of alignment in query
    case object qend      extends OutputField[Int]
    // means Start of alignment in subject
    case object sstart    extends OutputField[Int]
    // means End of alignment in subject
    case object send      extends OutputField[Int]
    // means Aligned part of query sequence
    case object qseq      extends OutputField[String]
    // means Aligned part of subject sequence
    case object sseq      extends OutputField[String]
    // means Expect value
    case object evalue    extends OutputField[Double]
    // means Bit score
    case object bitscore  extends OutputField[Long]
    // means Raw score
    case object score     extends OutputField[Long]
    // means Alignment length
    case object length    extends OutputField[Int]
    // means Percentage of identical matches
    case object pident    extends OutputField[Double]
    // means Number of identical matches
    // case object nident extends OutputField[String]  {
    // }
    // means Number of mismatches
    case object mismatch  extends OutputField[Int]
    // means Number of positive-scoring matches
    case object positive  extends OutputField[Int]
    // means Number of gap openings
    case object gapopen   extends OutputField[Int]
    // means Total number of gaps
    case object gaps      extends OutputField[Int]
    // case object ppos extends OutputField[String]  { // means Percentage of positive-scoring matches
    // }
    // case object frames extends OutputField[String]  { // means Query and subject frames separated by a '/'
    // }
    // means Query frame
    case object qframe      extends OutputField[String]
    // means Subject frame
    case object sframe      extends OutputField[String]
    // case object btop extends OutputField[String]  { // means Blast traceback operations (BTOP)
    // }
    // case object staxids extends OutputField[String]  { // means unique Subject Taxonomy ID(s), separated by a ';' (in numerical order)
    // }
    // case object sscinames extends OutputField[String]  { // means unique Subject Scientific Name(s), separated by a ';'
    // }
    // case object scomnames extends OutputField[String]  { // means unique Subject Common Name(s), separated by a ';'
    // }
    // case object sblastnames extends OutputField[String]  { // means unique Subject Blast Name(s), separated by a ';' (in alphabetical order)
    // }
    // case object sskingdoms extends OutputField[String]  { // means unique Subject Super Kingdom(s), separated by a ';' (in alphabetical order)
    // }
    // case object stitle extends OutputField[String]  { // means Subject Title
    // }
    // case object salltitles extends OutputField[String]  { // means All Subject Title(s), separated by a '<>'
    // }
    // case object sstrand extends OutputField[String]  { // means Subject Strand
    // }
    // case object qcovs extends OutputField[String]  { // means Query Coverage Per Subject
    // }
    // case object qcovhsp extends OutputField[String]  { // means Query Coverage Per HSP
    // }
  }

  import outFields._

  val defaultOutputFields = qseqid      :~:
                            sseqid      :~:
                            pident      :~:
                            length      :~:
                            mismatch    :~:
                            gapopen     :~:
                            qstart      :~:
                            qend        :~:
                            sstart      :~:
                            send        :~:
                            outFields.evalue  :~:
                            bitscore    :~: ∅



                            // case class BlastStatement[
                            //   Cmd <: AnyBlastCommand,
                            //   Opts <: AnyTypeSet.Of[AnyBlastOption]
                            // ](
                            //   val command: Cmd,
                            //   val options: Opts
                            // )(implicit
                            //   val ev: CheckForAll[Opts, OptionFor[Cmd]],
                            //   val toListEv: ToListOf[Opts, AnyBlastOption],
                            //   val allArgs: Cmd#Arguments ⊂ Opts
                            // )
                            // {
                            //   def toSeq: Seq[String] =  Seq(command.name) ++
                            //                             ( (options.toListOf[AnyBlastOption]) flatMap { _.toSeq } )
                            // }
                            //
                            // implicit def getBlastCommandOps[BC <: AnyBlastCommand](cmd: BC): BlastCommandOps[BC] =
                            //   BlastCommandOps(cmd)
                            //
                            // case class BlastCommandOps[Cmd <: AnyBlastCommand](val cmd: Cmd) {
                            //
                            //   def withOptions[
                            //     Opts <: AnyTypeSet.Of[AnyBlastOption]
                            //   ](opts: Opts)(implicit
                            //     ev: CheckForAll[Opts, OptionFor[Cmd]],
                            //     toListEv: ToListOf[Opts, AnyBlastOption],
                            //     allArgs: Cmd#Arguments ⊂ Opts
                            //   ): BlastStatement[Cmd,Opts] = BlastStatement(cmd, opts)
                            // }
}

```




[test/scala/CommandGeneration.scala]: ../../test/scala/CommandGeneration.scala.md
[main/scala/blast.scala]: blast.scala.md