package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, klists._

// type makeblastdb = makeblastdb.type
case object makeblastdb extends AnyBlastCommand {

  case object arguments extends RecordType(in :×: input_type :×: dbtype :×: out :×: |[AnyBlastOption])
  type Arguments = arguments.type

  // TODO: figure out the full list of options here
  case object options extends RecordType(title :×: parse_seqids :×: |[AnyBlastOption])
  type Options = options.type

  type ValidOutputFields = |[AnyOutputField]

  type ArgumentsVals =
    (in.type         := in.Raw)         ::
    (input_type.type := input_type.Raw) ::
    (dbtype.type     := dbtype.Raw)     ::
    (out.type        := out.Raw)        ::
    *[AnyDenotation]

  type OptionsVals =
    (title.type := title.Raw)               ::
    (parse_seqids.type := parse_seqids.Raw) ::
    *[AnyDenotation]

  val defaults: Options := OptionsVals = options(
    title("")             ::
    parse_seqids(false)   ::
    *[AnyDenotation]
  )

  def apply(
    argumentValues: ArgumentsVals,
    optionValues: OptionsVals
  ): BlastExpression[this.type, BlastOutputRecord[|[AnyOutputField]]] =
    BlastExpression(this)(new BlastOutputRecord(|[AnyOutputField]), argumentValues, optionValues)
}
