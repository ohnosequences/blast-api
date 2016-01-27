package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, klists._

// type makeblastdb = makeblastdb.type
case object makeblastdb extends AnyBlastCommand {

  case object arguments extends RecordType(in :×: input_type :×: dbtype :×: |[AnyBlastOption])
  type Arguments = arguments.type

  case object options extends RecordType(title :×: |[AnyBlastOption])
  type Options = options.type

  // TODO: add ValidOutputFields

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

  def apply[R <: AnyBlastOutputRecord](
    outputRecord: R,
    argumentValues: ArgumentsVals,
    optionValues: OptionsVals
  )(implicit
    valid: R isValidOutputRecordFor this.type
  ): BlastExpression[this.type, R] = BlastExpression(this)(outputRecord, argumentValues, optionValues)
}
