package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, klists._
import ohnosequences.blast.api.outputFields._

// type blastx   = blastx.type
case object blastx extends AnyBlastCommand {

  case object arguments extends RecordType(db :×: query :×: out :×: |[AnyBlastOption])
  type Arguments = arguments.type

  // TODO: figure out the full list of options here
  case object options extends RecordType(
    num_threads     :×:
    evalue          :×:
    max_target_seqs :×:
    strand          :×:
    word_size       :×:
    ungapped        :×:
    |[AnyBlastOption]
  )
  type Options = options.type

  type ValidOutputFields =
    qseqid.type    :×:
    sseqid.type    :×:
    sgi.type       :×:
    qstart.type    :×:
    qend.type      :×:
    sstart.type    :×:
    send.type      :×:
    qlen.type      :×:
    slen.type      :×:
    bitscore.type  :×:
    score.type     :×:
    |[AnyOutputField]

  type ArgumentsVals =
    (db.type    := db.Raw)    ::
    (query.type := query.Raw) ::
    (out.type   := out.Raw)   ::
    *[AnyDenotation]

  type OptionsVals =
    (num_threads.type     := num_threads.Raw)     ::
    (evalue.type          := evalue.Raw)          ::
    (max_target_seqs.type := max_target_seqs.Raw) ::
    (strand.type          := strand.Raw)          ::
    (word_size.type       := word_size.Raw)       ::
    (ungapped.type        := ungapped.Raw)        ::
    *[AnyDenotation]

  val defaults: Options := OptionsVals = options (
    num_threads(1)          ::
    evalue(BigDecimal(10))  ::
    max_target_seqs(500)    ::
    strand(Strands.both)    ::
    word_size(4)            ::
    ungapped(false)         ::
    *[AnyDenotation]
  )

  case object task extends BlastOption[Task](t => t.name) {
    def apply(t: Task): this.type := Task = this := t
  }
  sealed abstract class Task(val name: String)
  case object blastx     extends Task( "blastx" )
  case object blastxFast extends Task( "blastx-fast" )

  def apply[R <: AnyBlastOutputRecord.For[this.type]](
    outputRecord: R,
    argumentValues: ArgumentsVals,
    optionValues: OptionsVals
  ): BlastExpression[this.type, R] = BlastExpression(this)(outputRecord, argumentValues, optionValues)
}
