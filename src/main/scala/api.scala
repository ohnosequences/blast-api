package ohnosequences.blast

case object api {

  import ohnosequences.cosas._, types._, records._, fns._, klists._, typeUnions._
  import java.io.File

  /*
    This trait models a command part of the `BLAST` suite, like `blastn`, `blastp`, or `makeblastdb`. It is a property, with values of that property being valid command expressions.
  */
  sealed trait AnyBlastCommand {

    /* This label should match the name of the command */
    lazy val name: Seq[String] = Seq(toString)

    // AnyRecordType { type Keys <: AnyProductType { type Bound <: AnyFlashOption } }

    type Arguments  <: AnyRecordType { type Keys <: AnyProductType { type Bound <: AnyBlastOption } }
    type Options    <: AnyRecordType { type Keys <: AnyProductType { type Bound <: AnyBlastOption } }

    // TODO move this out
    type OptionsVals <: Options#Raw
    /* default values for options; they are *optional*, so should have default values. */
    val defaults: Options := OptionsVals
    // val defaultsAsSeq: Seq[String]

    /* valid output fields for this command */
    type OutputFields <: AnyBlastOutputRecord

    // type Raw >: (ValueOf[Arguments], ValueOf[Options]) <: (ValueOf[Arguments], ValueOf[Options])
  }

  sealed trait AnyBlastOption extends AnyType {

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
  // trait AnyBlastOutputRecord extends AnyRecordType {
  //
  //   type PropertySet <: AnyPropertySet { type Properties <: AnyTypeSet.Of[AnyOutputField] }
  // }

  implicit def blastOutputRecordOps[OR <: AnyBlastOutputRecord](outputRec: OR): BlastOutputRecordOps[OR] =
    BlastOutputRecordOps(outputRec)
  case class BlastOutputRecordOps[OR <: AnyBlastOutputRecord](val outputRec: OR) extends AnyVal {

    def toSeq[MO <: AnyKList.withBound[String]](implicit
      canMap: AnyApp2At[MapKListOf[typeLabel.type,String], typeLabel.type, OR#Keys#Types] { type Y = MO }
    ): Seq[String] = {

      val fields: Seq[String] = (outputRec.keys.types: OR#Keys#Types) map typeLabel asList

      // '10' is the code for csv output
      Seq("-outfmt") :+ s"""10 ${fields.mkString(" ")}"""
    }
  }
  /*
    Given a BLAST command, we can choose an output record made out of output fields. Each command specifies through its `OutputFields` command which fields can be used for it; this is checked when you construct a `BlastExpression`.

    The object containing all the output fields contains parsers and serializers for all them.
  */
  // use the label for parsing the key afterwards
  // TODO add parsing
  sealed trait AnyOutputField extends AnyType

  trait OutputField[V] extends AnyOutputField {

    type Raw = V
    lazy val label: String = toString
  }

  type AnyBlastOutputRecord = AnyRecordType { type Keys <: AnyProductType { type Bound <: AnyOutputField } }
  type BlastOutputRecord[OFs <: AnyProductType { type Bound <: AnyOutputField }] = RecordType[OFs]

  class ValidOutputRecordFor[BC <: AnyBlastCommand] extends PredicateOver[AnyOutputField]

  case object ValidOutputRecordFor {

    implicit def trueIfOneOutputFields[
      BC <: AnyBlastCommand,
      O <: AnyOutputField
    ](implicit
      proof: O isOneOf BC#OutputFields#Keys#Types#Types
    ): ValidOutputRecordFor[BC] isTrueOn O =
      App1 { _: O => True }
  }

  /*
    ### `Seq[String]` Command generation

    for command values we generate a `Seq[String]` which is valid command expression that you can execute (assuming BLAST installed) using `scala.sys.process` or anything similar.
  */
  case object optionValueToSeq extends DepFn1[AnyDenotation, Seq[String]] {

    implicit def default[FO <: AnyBlastOption, V <: FO#Raw](implicit
      option: FO with AnyBlastOption { type Raw = FO#Raw }
    )
    : AnyApp1At[optionValueToSeq.type, FO := V] { type Y = Seq[String] }=
      App1 { v: FO := V => Seq(option.label) ++ option.valueToString(v.value).filterNot(_.isEmpty) }
  }

  trait AnyBlastExpressionType {

    type Command <: AnyBlastCommand
    val command: Command

    type OutputRecord <: AnyBlastOutputRecord
    val outputRecord: OutputRecord

    // TODO fix this!
    // val validRecord: OutputRecord#Keys CheckForAll ValidOutputRecordFor[Command]
  }

  abstract class BlastExpressionType[
    BC <: AnyBlastCommand,
    OR <: AnyBlastOutputRecord
  ](
    val command: BC
  )(
    val outputRecord: OR
  )
  // TODO fix this!
  // (implicit
  //   val validRecord: OR#Keys CheckForAll ValidOutputRecordFor[BC]
  // )
  extends AnyBlastExpressionType {

    type Command = BC
    type OutputRecord = OR
  }

  trait AnyBlastExpression {

    type Tpe <: AnyBlastExpressionType
    val tpe: Tpe

    type OptionVals <: Tpe#Command#Options#Raw
    type ArgumentVals <: Tpe#Command#Arguments#Raw

    val optionValues: Tpe#Command#Options := OptionVals
    val argumentValues: Tpe#Command#Arguments := ArgumentVals
  }

  case class BlastExpression[
    T <: AnyBlastExpressionType,
    OV <: T#Command#Options#Raw,
    AV <: T#Command#Arguments#Raw
  ](
    val tpe: T
  )(
    val optionValues: T#Command#Options := OV,
    val argumentValues: T#Command#Arguments := AV
  )
  extends AnyBlastExpression {

    type Tpe = T
    type OptionVals = OV
    type ArgumentVals = AV
  }

  implicit def blastExpressionOps[Expr <: AnyBlastExpression](expr: Expr): BlastExpressionOps[Expr] =
    BlastExpressionOps(expr)
  case class BlastExpressionOps[Expr <: AnyBlastExpression](val expr: Expr) extends AnyVal {

    def cmd[
      MA <: AnyKList.withBound[Seq[String]],
      MO <: AnyKList.withBound[Seq[String]],
      MOP <: AnyKList.withBound[String]
    ](implicit
      mapArgs: AnyApp2At[MapKListOf[optionValueToSeq.type,Seq[String]], optionValueToSeq.type, Expr#ArgumentVals] { type Y = MA },
      mapOpts: AnyApp2At[MapKListOf[optionValueToSeq.type,Seq[String]], optionValueToSeq.type, Expr#OptionVals] { type Y = MO },
      mapOutputProps: AnyApp2At[MapKListOf[typeLabel.type,String], typeLabel.type, Expr#Tpe#OutputRecord#Keys#Types] { type Y = MOP }
      // mapOutputProps: (typeLabel.type MapToList Expr#Tpe#OutputRecord#Properties) { type O = String }
    ): Seq[String] = {

      val (argsSeqs, optsSeqs): (Seq[Seq[String]], Seq[Seq[String]]) = (
        (expr.argumentValues.value: Expr#ArgumentVals).map(optionValueToSeq).asList,
        (expr.optionValues.value: Expr#OptionVals).map(optionValueToSeq).asList
      )

      expr.tpe.command.name ++
      argsSeqs.toSeq.flatten ++
      optsSeqs.toSeq.flatten ++
      (expr.tpe.outputRecord: Expr#Tpe#OutputRecord).toSeq
    }
  }

  /*
    ### BLAST command instances

    All the BLAST suite commands, together with their arguments, options and default values.
  */
  type blastn = blastn.type
  case object blastn extends AnyBlastCommand {

    type Arguments = arguments.type
    case object arguments extends RecordType(db :×: query :×: out :×: In[AnyBlastOption])
    type Options = options.type
    case object options extends RecordType(
      num_threads :×:
      task        :×:
      evalue      :×:
      max_target_seqs :×:
      strand      :×:
      word_size   :×:
      show_gis    :×:
      ungapped    :×: In[AnyBlastOption]
    )

    import ohnosequences.blast.api.outputFields._

    type OutputFields = outputFields.type
    case object outputFields extends RecordType(
      qseqid      :×:
      sseqid      :×:
      sgi         :×:
      qstart      :×:
      qend        :×:
      sstart      :×:
      send        :×:
      qlen        :×:
      slen        :×:
      bitscore    :×:
      score       :×: In[AnyOutputField]
    )

    type OptionsVals = (num_threads.type      := num_threads.Raw)      ::
                      (task.type             := task.Raw)             ::
                      (api.evalue.type       := api.evalue.Raw)       ::
                      (max_target_seqs.type  := max_target_seqs.Raw)  ::
                      (strand.type           := strand.Raw)           ::
                      (word_size.type        := word_size.Raw)        ::
                      (show_gis.type         := show_gis.Raw)         ::
                      (ungapped.type         := ungapped.Raw)         :: *[AnyDenotation]

    val defaults: Options := OptionsVals = options := (
      num_threads(1)                ::
      task(blastn)                  ::
      api.evalue(10: Double)        ::
      max_target_seqs(100)          ::
      strand(Strands.both)          ::
      word_size(4)                  ::
      show_gis(false)               ::
      ungapped(false)               :: *[AnyDenotation]
    )

    lazy val defaultsAsSeq = (defaults.value map optionValueToSeq).asList.flatten

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

    case object arguments extends RecordType(db :×: query :×: out :×: In[AnyBlastOption])
    type Arguments = arguments.type
    case object options extends RecordType(num_threads :×: In[AnyBlastOption])
    type Options = options.type

    val defaults = options(
      num_threads(1) :: *[AnyDenotation]
    )

    lazy val defaultsAsSeq = (defaults.value map optionValueToSeq asList).flatten

    case object task extends BlastOption[Task](t => t.name)
    sealed abstract class Task(val name: String)
    case object blastp      extends Task( "blastp" )
    case object blastpFast  extends Task( "blastp-fast" )
    case object blastpShort extends Task( "blastp-short" )
  }

  type blastx   = blastx.type
  case object blastx extends AnyBlastCommand {

    case object arguments extends RecordType(db :×: query :×: out :×: In[AnyBlastOption])
    type Arguments = arguments.type
    case object options extends RecordType(num_threads :×: In[AnyBlastOption])
    type Options = options.type

    val defaults = options(
      num_threads(1) :: *[AnyDenotation]
    )

    lazy val defaultsAsSeq = (defaults.value map optionValueToSeq).asList.flatten

    case object task extends BlastOption[Task](t => t.name)
    sealed abstract class Task(val name: String)
    case object blastx     extends Task( "blastx" )
    case object blastxFast extends Task( "blastx-fast" )
  }

  type tblastn = tblastn.type
  case object tblastn extends AnyBlastCommand {

    case object arguments extends RecordType(db :×: query :×: out :×: In[AnyBlastOption])
    type Arguments = arguments.type
    case object options extends RecordType(num_threads :×: In[AnyBlastOption])
    type Options = options.type

    val defaults = options := num_threads(1) :: *[AnyDenotation]

    lazy val defaultsAsSeq = (defaults.value map optionValueToSeq asList).flatten

    case object task extends BlastOption[Task](t => t.name)
    sealed abstract class Task(val name: String)
    case object tblastn     extends Task( "tblastn" )
    case object tblastnFast extends Task( "tblastn-fast" )
  }

  type tblastx = tblastx.type
  case object tblastx extends AnyBlastCommand {

    case object arguments extends RecordType(db :×: query :×: out :×: In[AnyBlastOption])
    type Arguments = arguments.type
    case object options extends RecordType(num_threads :×: In[AnyBlastOption])
    type Options = options.type

    val defaults = options := num_threads(1) :: *[AnyDenotation]

    lazy val defaultsAsSeq = (defaults.value map optionValueToSeq asList).flatten
  }

  type makeblastdb = makeblastdb.type
  case object makeblastdb extends AnyBlastCommand {

    case object arguments extends RecordType(in :×: input_type :×: dbtype :×: In[AnyBlastOption])
    type Arguments = arguments.type
    case object options extends RecordType(title :×: In[AnyBlastOption])
    type Options = options.type

    val defaults = options := title("") :: *[AnyDenotation]

    lazy val defaultsAsSeq = (defaults.value map optionValueToSeq asList).flatten
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

    val longParser: String => Option[Long] = str => {
      import scala.util.control.Exception._
      catching(classOf[NumberFormatException]) opt str.toLong
    }

    val doubleFromScientificNotation: String => Option[Double] = str => {
      import java.math.BigDecimal
      // funny blast output
      Some( (new BigDecimal( str.replace("e", "E") )).doubleValue )
    }

    /* Query Seq-id */
    type qseqid = qseqid.type
    case object qseqid    extends OutputField[String]
    implicit val qseqidParser: DenotationParser[qseqid,String,String] =
      new DenotationParser(qseqid, qseqid.label)({ s: String => Some(s) })
    implicit val qseqidSerializer: DenotationSerializer[qseqid,String,String] =
      new DenotationSerializer(qseqid, qseqid.label)({ v: String => Some(v) })

    /* Query GI */
    case object qgi       extends OutputField[String]
    // means Query accesion
    case object qacc      extends OutputField[String]
    // means Query accesion.version
    case object qaccver   extends OutputField[Int]

    /* Query sequence length */
    type qlen = qlen.type
    case object qlen      extends OutputField[Int]
    implicit val qlenParser: DenotationParser[qlen,Int,String] =
      new DenotationParser(qlen, qlen.label)({ intParser })
    implicit val qlenSerializer: DenotationSerializer[qlen,Int,String] =
      new DenotationSerializer(qlen, qlen.label)({ v => Some(v.toString) })

    /* Subject Seq-id */
    type sseqid = sseqid.type
    case object sseqid    extends OutputField[String]
    implicit val sseqidParser: DenotationParser[sseqid,String,String] =
      new DenotationParser(sseqid, sseqid.label)({ s: String => Some(s) })
    implicit val sseqidSerializer: DenotationSerializer[sseqid,String,String] =
      new DenotationSerializer(sseqid, sseqid.label)({ v: String => Some(v) })


    // means All subject Seq-id(s), separated by a ';'
    case object sallseqid extends OutputField[List[String]]

    /* Subject GI */
    type sgi = sgi.type
    case object sgi       extends OutputField[String]
    implicit val sgiParser: DenotationParser[sgi,String,String] =
      new DenotationParser(sgi, sgi.label)({ s: String => Some(s) })
    implicit val sgiSerializer: DenotationSerializer[sgi,String,String] =
      new DenotationSerializer(sgi, sgi.label)({ v: String => Some(v) })

    // means All subject GIs
    case object sallgi    extends OutputField[List[String]]
    // means Subject accession
    case object sacc      extends OutputField[String]
    // means Subject accession.version
    case object saccver   extends OutputField[String]
    // means All subject accessions
    case object sallacc   extends OutputField[String]

    /* Subject sequence length */
    type slen = slen.type
    case object slen      extends OutputField[Int]
    implicit val slenParser: DenotationParser[slen,Int,String] =
      new DenotationParser(slen, slen.label)(intParser)
    implicit val slenSerializer: DenotationSerializer[slen,Int,String] =
      new DenotationSerializer(slen, slen.label)({ v => Some(v.toString) })


    /* Start of alignment in query */
    type qstart = qstart.type
    case object qstart    extends OutputField[Int]
    implicit val qstartParser: DenotationParser[qstart,Int,String] =
      new DenotationParser(qstart, qstart.label)(intParser)
    implicit val qstartSerializer: DenotationSerializer[qstart,Int,String] =
      new DenotationSerializer(qstart, qstart.label)({ v => Some(v.toString) })

    /* End of alignment in query */
    type qend = qend.type
    case object qend      extends OutputField[Int]
    implicit val qendParser: DenotationParser[qend,Int,String] =
      new DenotationParser(qend, qend.label)(intParser)
    implicit val qendSerializer: DenotationSerializer[qend,Int,String] =
      new DenotationSerializer(qend, qend.label)({ v => Some(v.toString) })

    /* Start of alignment in subject */
    type sstart = sstart.type
    case object sstart    extends OutputField[Int]
    implicit val sstartParser: DenotationParser[sstart,Int,String] =
    new DenotationParser(sstart, sstart.label)(intParser)
    implicit val sstartSerializer: DenotationSerializer[sstart,Int,String] =
      new DenotationSerializer(sstart, sstart.label)({ v => Some(v.toString) })

    /* End of alignment in subject */
    type send = send.type
    case object send      extends OutputField[Int]
    implicit val sendParser: DenotationParser[send,Int,String] =
      new DenotationParser(send, send.label)(intParser)
    implicit val sendSerializer: DenotationSerializer[send,Int,String] =
      new DenotationSerializer(send, send.label)({ v => Some(v.toString) })

    // means Aligned part of query sequence
    type qseq = qseq.type
    case object qseq      extends OutputField[String]

    // means Aligned part of subject sequence
    type sseq = sseq.type
    case object sseq      extends OutputField[String]

    // means Expect value
    case object evalue    extends OutputField[Double]
    // TODO this does not seem to work as expected
    implicit val evalueParser: DenotationParser[evalue.type,Double,String] =
      new DenotationParser(evalue, evalue.label)(doubleFromScientificNotation)
    implicit val evalueSerializer: DenotationSerializer[evalue.type,Double,String] =
      new DenotationSerializer(evalue, evalue.label)({ v => Some(v.toString) })

    // means Bit score
    type bitscore = bitscore.type
    case object bitscore  extends OutputField[Long]
    implicit val bitscoreParser: DenotationParser[bitscore,Long,String] =
      new DenotationParser(bitscore, bitscore.label)(longParser)
    implicit val bitscoreSerializer: DenotationSerializer[bitscore,Long,String] =
      new DenotationSerializer(bitscore, bitscore.label)({ v => Some(v.toString) })

    // means Raw score
    type score = score.type
    case object score     extends OutputField[Long]
    implicit val scoreParser: DenotationParser[score,Long,String] =
      new DenotationParser(score, score.label)(longParser)
    implicit val scoreSerializer: DenotationSerializer[score,Long,String] =
      new DenotationSerializer(score, score.label)({ v => Some(v.toString) })

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
