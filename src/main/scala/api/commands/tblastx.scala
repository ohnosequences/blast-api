package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, klists._

// type tblastx = tblastx.type
case object tblastx extends AnyBlastCommand {

  case object arguments extends RecordType(db :×: query :×: out :×: |[AnyBlastOption])
  type Arguments = arguments.type

  case object options extends RecordType(num_threads :×: |[AnyBlastOption])
  type Options = options.type

  // TODO: add ValidOutputFields

  type ArgumentsVals =
    (db.type    := db.Raw)    ::
    (query.type := query.Raw) ::
    (out.type   := out.Raw)   ::
    *[AnyDenotation]

  type OptionsVals =
    (num_threads.type := num_threads.Raw) ::
    *[AnyDenotation]

  val defaults = options(
    num_threads(1) ::
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
