package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, klists._

// type blastn = blastn.type
case object blastn extends AnyBlastCommand {

  type Arguments = arguments.type
  case object arguments extends RecordType(db :×: query :×: out :×: |[AnyBlastOption])

  type Options = options.type
  case object options extends RecordType(
    num_threads     :×:
    task            :×:
    evalue          :×:
    max_target_seqs :×:
    strand          :×:
    word_size       :×:
    show_gis        :×:
    ungapped        :×:
    |[AnyBlastOption]
  )

  type ArgumentsVals =
    (db.type    := db.Raw)    ::
    (query.type := query.Raw) ::
    (out.type   := out.Raw)   ::
    *[AnyDenotation]

  type OptionsVals =
    (num_threads.type     := num_threads.Raw)     ::
    (task.type            := task.Raw)            ::
    (evalue.type          := evalue.Raw)          ::
    (max_target_seqs.type := max_target_seqs.Raw) ::
    (strand.type          := strand.Raw)          ::
    (word_size.type       := word_size.Raw)       ::
    (show_gis.type        := show_gis.Raw)        ::
    (ungapped.type        := ungapped.Raw)        ::
    *[AnyDenotation]

  val defaults = options (
    num_threads(1)          ::
    task(blastn)            ::
    evalue(10D)             ::
    max_target_seqs(100)    ::
    strand(Strands.both)    ::
    word_size(4)            ::
    show_gis(false)         ::
    ungapped(false)         ::
    *[AnyDenotation]
  )

  import ohnosequences.blast.api.outputFields._

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

  // task depends on each command, that's why it is here.
  case object task extends BlastOption[Task](t => t.name)
  sealed abstract class Task(val name: String)
  case object megablast       extends Task( "megablast" )
  case object dcMegablast     extends Task( "dc-megablast" )
  case object blastn          extends Task( "blastn" )
  case object blastnShort     extends Task( "blastn-short" )
  case object rmblastn        extends Task( "rmblastn" )

  def apply[R <: AnyBlastOutputRecord](
    outputRecord: R,
    argumentValues: ArgumentsVals,
    optionValues: OptionsVals
  )(implicit
    valid: R isValidOutputRecordFor this.type
  ): BlastExpression[this.type, R] = BlastExpression(this)(outputRecord, argumentValues, optionValues)
}
