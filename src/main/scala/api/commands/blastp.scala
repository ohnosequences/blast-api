package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, klists._
import ohnosequences.blast.api.outputFields._

case object blastp extends AnyBlastCommand {

  case object arguments extends RecordType(db :×: query :×: out :×: |[AnyBlastOption])
  type Arguments = arguments.type

  type Options = options.type
  case object options extends RecordType(
    num_threads     :×:
    evalue          :×:
    show_gis        :×:
    max_target_seqs :×:
    word_size       :×:
    |[AnyBlastOption]
  )

  type ArgumentsVals =
    (db.type    := db.Raw)    ::
    (query.type := query.Raw) ::
    (out.type   := out.Raw)   ::
    *[AnyDenotation]

  type OptionsVals =
    (num_threads.type     := num_threads.Raw)     ::
    (evalue.type          := evalue.Raw)          ::
    (show_gis.type        := show_gis.Raw)        ::
    (max_target_seqs.type := max_target_seqs.Raw) ::
    (word_size.type       := word_size.Raw)       ::
    *[AnyDenotation]

  /* Default values match those documented in [the official BLAST docs](http://www.ncbi.nlm.nih.gov/books/NBK279675/) whenever possible. */
  val defaults: Options := OptionsVals = options (
    num_threads(1)        ::
    evalue(BigDecimal(10))::
    show_gis(false)       ::
    max_target_seqs(500)  ::
    word_size(3)          :: // valid word sizes are 2-7
    *[AnyDenotation]
  )

  type ValidOutputFields =
    qseqid.type     :×:
    sseqid.type     :×:
    sgi.type        :×:
    qstart.type     :×:
    qend.type       :×:
    sstart.type     :×:
    send.type       :×:
    qlen.type       :×:
    slen.type       :×:
    qseq.type       :×:
    sseq.type       :×:
    outputFields.evalue.type :×:
    bitscore.type   :×:
    score.type      :×:
    length.type     :×:
    pident.type     :×:
    mismatch.type   :×:
    positive.type   :×:
    gapopen.type    :×:
    gaps.type       :×:
    qcovs.type      :×:
    nident.type     :×:
    ppos.type       :×:
    |[AnyOutputField]

  def apply[R <: AnyBlastOutputRecord.For[this.type]](
    outputRecord: R,
    argumentValues: ArgumentsVals,
    optionValues: OptionsVals
  ): BlastExpression[this.type, R] = BlastExpression(this)(outputRecord, argumentValues, optionValues)
}
