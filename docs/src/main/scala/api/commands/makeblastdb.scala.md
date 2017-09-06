
```scala
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

```




[test/scala/CommandGeneration.scala]: ../../../../test/scala/CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: ../../../../test/scala/OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: ../../../../test/scala/OutputFieldsSpecification.scala.md
[test/scala/igblastn.scala]: ../../../../test/scala/igblastn.scala.md
[main/scala/api/outputFields.scala]: ../outputFields.scala.md
[main/scala/api/options.scala]: ../options.scala.md
[main/scala/api/package.scala]: ../package.scala.md
[main/scala/api/expressions.scala]: ../expressions.scala.md
[main/scala/api/parse/igblastn.scala]: ../parse/igblastn.scala.md
[main/scala/api/commands/blastn.scala]: blastn.scala.md
[main/scala/api/commands/blastp.scala]: blastp.scala.md
[main/scala/api/commands/tblastx.scala]: tblastx.scala.md
[main/scala/api/commands/tblastn.scala]: tblastn.scala.md
[main/scala/api/commands/blastx.scala]: blastx.scala.md
[main/scala/api/commands/makeblastdb.scala]: makeblastdb.scala.md
[main/scala/api/commands/igblastn.scala]: igblastn.scala.md