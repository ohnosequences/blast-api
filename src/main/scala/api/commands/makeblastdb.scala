package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, klists._

// type makeblastdb = makeblastdb.type
case object makeblastdb extends AnyBlastCommand {

  case object arguments extends RecordType(in :×: input_type :×: dbtype :×: |[AnyBlastOption])
  type Arguments = arguments.type

  // TODO: figure out the full list of options here
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

  val defaults: Options := OptionsVals = options (
    title("") ::
    *[AnyDenotation]
  )

  // This does not extends AnyBlastExpression, because it doesn't have an output record
  case class MakeBlastDBExpression(
    argumentValues: ArgumentsVals,
    optionValues: OptionsVals
  ) {

    def toSeq: Seq[String] =
      makeblastdb.name ++
      argumentValues.toSeq ++
      optionValues.toSeq
  }

  def apply(
    argumentValues: ArgumentsVals,
    optionValues: OptionsVals
  ): MakeBlastDBExpression =
     MakeBlastDBExpression(argumentValues, optionValues)
}
