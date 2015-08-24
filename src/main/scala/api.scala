package ohnosequences.blast

case object api {

  import ohnosequences.cosas._, typeSets._, properties._, types._, records._
  import ohnosequences.cosas.ops.typeSets.{ CheckForAll, ToList, MapToList }
  import shapeless.poly._
  import java.io.File

  /*
    This trait models a command part of the `BLAST` suite, like `blastn`, `blastp`, or `makeblastdb`. It is a property, with values of that property being valid command expressions.
  */
  sealed trait AnyBlastCommand {

    /* This label should match the name of the command */
    lazy val name: Seq[String] = Seq(toString)

    type Arguments  <: AnyRecord { type PropertySet <: AnyPropertySet.withBound[AnyBlastOption] }
    type Options    <: AnyRecord { type PropertySet <: AnyPropertySet.withBound[AnyBlastOption] }

    /* default values for options; they are *optional*, so should have default values. */
    val defaults: ValueOf[Options]
    // val defaultsAsSeq: Seq[String]

    /* valid output fields for this command */
    type OutputFields <: AnyPropertySet { type Properties <: AnyTypeSet.Of[AnyOutputField] }

    // type Raw >: (ValueOf[Arguments], ValueOf[Options]) <: (ValueOf[Arguments], ValueOf[Options])
  }

  sealed trait AnyBlastOption extends AnyProperty {

    /* The `label` is used for generating the command-line `String` representation of this option. For a BLAST option `-x_yz_abc` name your option here `case object x_yz_abc`. */
    lazy val label: String = s"-${toString}"

    /* this is used for serializing values to command-line args */
    val valueToString: Raw => Seq[String]
  }
  case object AnyBlastOption {

    type is[B <: AnyBlastOption] = B with AnyBlastOption { type Raw = B#Raw }
  }
  abstract class BlastOption[V](val v: V => String) extends AnyBlastOption {

    type Raw = V

    val valueToString = { x: V => Seq(v(x)) }
  }

  /*
    ### BLAST output formats and fields

    A lot of different outputs, plus the possibility of choosing fields for CSV/TSV output.
  */
  trait AnyBlastOutputRecord extends AnyRecord {

    type PropertySet <: AnyPropertySet { type Properties <: AnyTypeSet.Of[AnyOutputField] }
  }
  abstract class BlastOutputRecord[
    PS <: AnyPropertySet { type Properties <: AnyTypeSet.Of[AnyOutputField] }
  ](val propertySet: PS) extends AnyBlastOutputRecord {

    type PropertySet = PS
    lazy val label = toString
  }

  // TODO move to cosas, actually to `AnyType`
  case object propertyLabel extends shapeless.Poly1 {

    implicit def default[P <: AnyProperty] = at[P]{ p: P => p.label }
  }
  implicit def blastOutputRecordOps[OR <: AnyBlastOutputRecord](outputRec: OR): BlastOutputRecordOps[OR] =
    BlastOutputRecordOps(outputRec)
  case class BlastOutputRecordOps[OR <: AnyBlastOutputRecord](val outputRec: OR) extends AnyVal {

    def toSeq(implicit
      canMap: (propertyLabel.type MapToList OR#Properties) { type O = String }
    ): Seq[String] = {

      val fields: String = ((outputRec.properties: OR#Properties) mapToList propertyLabel).mkString(" ")

      // '10' is the code for csv output
      Seq("-outfmt") :+ s"'10 ${fields}'"
    }
  }
  /*
    Given a BLAST command, we can choose an output record made out of output fields. Each command specifies through its `OutputFields` command which fields can be used for it; this is checked when you construct a `BlastExpression`.

    The object containing all the output fields contains parsers and serializers for all them.
  */
  // use the label for parsing the key afterwards
  // TODO add parsing
  sealed trait AnyOutputField extends AnyProperty

  trait OutputField[V] extends AnyOutputField {

    type Raw = V
    lazy val label: String = toString
  }

  trait ValidOutputRecordFor[BC <: AnyBlastCommand] extends TypePredicate[AnyOutputField] {

    type Condition[OF <: AnyOutputField] = OF isIn BC#OutputFields#Properties
  }

  /*
    ### `Seq[String]` Command generation

    for command values we generate a `Seq[String]` which is valid command expression that you can execute (assuming BLAST installed) using `scala.sys.process` or anything similar.
  */
  case object optionValueToSeq extends shapeless.Poly1 {

    implicit def default[BO <: AnyBlastOption](implicit option: AnyBlastOption.is[BO]) =
      at[ValueOf[BO]]{ v: ValueOf[BO] => Seq(option.label) ++ option.valueToString(v.value).filterNot(_.isEmpty) }
  }

  trait AnyBlastExpression {

    type Command <: AnyBlastCommand
    val command: Command
    // TODO a more succint bound
    type OutputRecord <: AnyBlastOutputRecord
    val outputRecord: OutputRecord

    val optionValues: ValueOf[Command#Options]
    val argumentValues: ValueOf[Command#Arguments]

    val validRecord: OutputRecord#Properties CheckForAll ValidOutputRecordFor[Command]
  }

  case class BlastExpression[
    BC <: AnyBlastCommand,
    OR <: AnyBlastOutputRecord
  ](
    val command: BC
  )(
    val outputRecord: OR
  )(
    val optionValues: ValueOf[BC#Options],
    val argumentValues: ValueOf[BC#Arguments]
  )(implicit
    val validRecord: OR#Properties CheckForAll ValidOutputRecordFor[BC]
  )
  extends AnyBlastExpression {

    type Command = BC
    type OutputRecord = OR
  }

  implicit def blastExpressionOps[Expr <: AnyBlastExpression](expr: Expr): BlastExpressionOps[Expr] =
    BlastExpressionOps(expr)
  case class BlastExpressionOps[Expr <: AnyBlastExpression](val expr: Expr) extends AnyVal {

    def cmd(implicit
      mapArgs: (optionValueToSeq.type MapToList Expr#Command#Arguments#Raw) { type O = Seq[String] },
      mapOpts: (optionValueToSeq.type MapToList Expr#Command#Options#Raw) { type O = Seq[String] },
      mapOutputProps: (propertyLabel.type MapToList Expr#OutputRecord#Properties) { type O = String }
    ): Seq[String] = {

      val (argsSeqs, optsSeqs): (List[Seq[String]], List[Seq[String]]) = (
        (expr.argumentValues.value: Expr#Command#Arguments#Raw) mapToList optionValueToSeq,
        (expr.optionValues.value: Expr#Command#Options#Raw)     mapToList optionValueToSeq
      )

      expr.command.name ++
      argsSeqs.toSeq.flatten ++
      optsSeqs.toSeq.flatten ++
      (expr.outputRecord: Expr#OutputRecord).toSeq
    }
  }

  /*
    ### BLAST command instances

    All the BLAST suite commands, together with their arguments, options and default values.
  */
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

    import ohnosequences.blast.api.outputFields.{qseqid, sseqid}
    type OutputFields = qseqid :&: sseqid :&: □
    val outputFields: OutputFields = qseqid :&: sseqid :&: □

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

  /*
    ### Options

    As the same options are valid for several commands, they are defined independently here.
  */
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
  /*
    #### `makeblastdb`-specific options
  */
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

  /* Inside this object you have all the possible fields that you can specify as output */
  // TODO move this to a different file
  case object outputFields {

    // parsers
    val intParser: String => Option[Int] = str => {
      import scala.util.control.Exception._
      catching(classOf[NumberFormatException]) opt str.toInt
    }

    val doubleFromScientificNotation: String => Option[Double] = str => {
      import java.math.BigDecimal
      Some( (new BigDecimal(str)).doubleValue )
    }

    /* Query Seq-id */
    type qseqid = qseqid.type
    case object qseqid    extends OutputField[String]
    implicit val qseqidParser: PropertyParser[qseqid,String] =
      PropertyParser(qseqid, qseqid.label){ s: String => Some(s) }
    implicit val qseqidSerializer: PropertySerializer[qseqid,String] =
      PropertySerializer(qseqid, qseqid.label){ v: String => Some(v) }

    /* Query GI */
    case object qgi       extends OutputField[String]
    // means Query accesion
    case object qacc      extends OutputField[String]
    // means Query accesion.version
    case object qaccver   extends OutputField[Int]

    /* Query sequence length */
    type qlen = qlen.type
    case object qlen      extends OutputField[Int]
    implicit val qlenParser: PropertyParser[qlen,String] =
      PropertyParser(qlen, qlen.label){ intParser }
    implicit val qlenSerializer: PropertySerializer[qlen,String] =
      PropertySerializer(qlen, qlen.label){ v => Some(v.toString) }

    // means Subject Seq-id
    type sseqid = sseqid.type
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
    implicit val evalueParser: PropertyParser[evalue.type,String] =
      PropertyParser(evalue, evalue.label){ doubleFromScientificNotation }
    implicit val evalueSerializer: PropertySerializer[evalue.type,String] =
      PropertySerializer(evalue, evalue.label){ v => Some(v.toString) }

    // means Bit score
    case object bitscore  extends OutputField[Long]
    // means Raw score
    case object score     extends OutputField[Long]
    // means Alignment length
    case object length    extends OutputField[Int]
    // means Percentage of identical matches
    case object pident    extends OutputField[Double]
    // means Number of mismatches
    case object mismatch  extends OutputField[Int]
    // means Number of positive-scoring matches
    case object positive  extends OutputField[Int]
    // means Number of gap openings
    case object gapopen   extends OutputField[Int]
    // means Total number of gaps
    case object gaps      extends OutputField[Int]
    // means Query frame
    case object qframe    extends OutputField[String]
    // means Subject frame
    case object sframe    extends OutputField[String]

    // TODO sort these out
    // means Number of identical matches
    // case object nident extends OutputField[String]  {
    // }
    // case object ppos extends OutputField[String]  { // means Percentage of positive-scoring matches
    // }
    // case object frames extends OutputField[String]  { // means Query and subject frames separated by a '/'
    // }
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
}
