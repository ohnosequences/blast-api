package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, klists._

// type makeblastdb = makeblastdb.type
case object makeblastdb extends AnyBlastCommand {

  case object arguments extends RecordType(in :×: input_type :×: dbtype :×: |[AnyBlastOption])
  type Arguments = arguments.type

  case object options extends RecordType(title :×: |[AnyBlastOption])
  type Options = options.type

  type ValidOutputFields = |[AnyOutputField]

  type ArgumentsVals =
    (in.type         := in.Raw)    ::
    (input_type.type := input_type.Raw) ::
    (dbtype.type     := dbtype.Raw)   ::
    *[AnyDenotation]

  type OptionsVals =
    (title.type := title.Raw) ::
    *[AnyDenotation]

  val defaults = options (
    title("") ::
    *[AnyDenotation]
  )

  def apply(
    argumentValues: ArgumentsVals,
    optionValues: OptionsVals
  )(implicit
    valid: BlastOutputRecord[|[AnyOutputField]] isValidOutputRecordFor this.type
  ): BlastExpression[this.type, BlastOutputRecord[|[AnyOutputField]]] = BlastExpression(this)(new BlastOutputRecord(|[AnyOutputField]), argumentValues, optionValues)
}
