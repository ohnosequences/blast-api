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
    num_threads     :×:
    evalue          :×:
    max_target_seqs :×:
    strand          :×:
    word_size       :×:
    show_gis        :×:
    ungapped        :×:
    penalty         :×:
    reward          :×:
    perc_identity   :×:
    |[AnyBlastOption]
    // TODO add igblastn-specific options
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
    (num_threads.type     := num_threads.Raw)     ::
    (evalue.type          := evalue.Raw)          ::
    (max_target_seqs.type := max_target_seqs.Raw) ::
    (strand.type          := strand.Raw)          ::
    (word_size.type       := word_size.Raw)       ::
    (show_gis.type        := show_gis.Raw)        ::
    (ungapped.type        := ungapped.Raw)        ::
    (penalty.type         := penalty.Raw)         ::
    (reward.type          := reward.Raw)          ::
    (perc_identity.type   := perc_identity.Raw)   ::
    *[AnyDenotation]

  /* Default values match those documented in [the official BLAST docs](http://www.ncbi.nlm.nih.gov/books/NBK279675/) whenever possible. */
  val defaults: Options := OptionsVals = options (
    num_threads(1)        ::
    evalue(BigDecimal(10))::
    max_target_seqs(500)  ::
    strand(Strands.both)  ::
    word_size(11)         ::
    show_gis(false)       ::
    ungapped(false)       ::
    penalty(-3)           ::
    reward(2)             ::
    perc_identity(0D)     ::
    *[AnyDenotation]
  )

  /* `igblastn` does not support the csv output format */
  type ValidOutputFields =
    |[AnyOutputField]

  // TODO do something with this
  // def apply(
  //   argumentValues: ArgumentsVals,
  //   optionValues: OptionsVals
  // ): BlastExpression[this.type, ] = BlastExpression(this)(outputRecord, argumentValues, optionValues)
}
