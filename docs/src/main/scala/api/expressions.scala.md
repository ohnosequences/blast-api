
```scala
package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, fns._, klists._, typeUnions._
```


This trait models a command part of the `BLAST` suite, like `blastn`, `blastp`, or `makeblastdb`.

All the BLAST suite commands, together with their arguments, options and default values
are defined in separate files in the (commands/)[commands/] folder (but in the same `api` namespace).


```scala
// sealed
trait AnyBlastCommand {
```

This label **should** match the **name** of the command

```scala
  lazy val name: Seq[String] = Seq(this.toString)

  type Arguments <: AnyBlastOptionsRecord
  type Options   <: AnyBlastOptionsRecord

  type ArgumentsVals <: Arguments#Raw
  type OptionsVals   <: Options#Raw
```

default values for options; they are *optional*, so should have default values.

```scala
  val defaults: Options := OptionsVals
```

valid output fields for this command

```scala
  type ValidOutputFields <: AnyBlastOutputFields
}
```


An expression is a BLAST command with all the needed arguments given and ready to be executed


```scala
trait AnyBlastExpression {

  type Command <: AnyBlastCommand
  val  command: Command

  type OutputRecord <: AnyBlastOutputRecord.For[Command]
  val  outputRecord: OutputRecord

  val argumentValues: Command#ArgumentsVals
  val optionValues: Command#OptionsVals

  // implicitly:
  val argValsToSeq: BlastOptionsToSeq[Command#ArgumentsVals]
  val optValsToSeq: BlastOptionsToSeq[Command#OptionsVals]
```

For command values we generate a `Seq[String]` which is valid command expression that you can
execute (assuming BLAST installed) using `scala.sys.process` or anything similar.

```scala
  def toSeq: Seq[String] =
    command.name ++
    argValsToSeq(argumentValues) ++
    optValsToSeq(optionValues) ++
    outputRecord.toSeq
}

case class BlastExpression[
  C <: AnyBlastCommand,
  R <: AnyBlastOutputRecord.For[C]
](val command: C
)(val outputRecord: R,
  val argumentValues: C#ArgumentsVals,
  val optionValues: C#OptionsVals
)(implicit
  val argValsToSeq: BlastOptionsToSeq[C#ArgumentsVals],
  val optValsToSeq: BlastOptionsToSeq[C#OptionsVals]
) extends AnyBlastExpression {

  type Command = C
  type OutputRecord = R
}

```




[test/scala/CommandGeneration.scala]: ../../../test/scala/CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: ../../../test/scala/OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: ../../../test/scala/OutputFieldsSpecification.scala.md
[main/scala/api/outputFields.scala]: outputFields.scala.md
[main/scala/api/options.scala]: options.scala.md
[main/scala/api/package.scala]: package.scala.md
[main/scala/api/expressions.scala]: expressions.scala.md
[main/scala/api/commands/blastn.scala]: commands/blastn.scala.md
[main/scala/api/commands/blastp.scala]: commands/blastp.scala.md
[main/scala/api/commands/tblastx.scala]: commands/tblastx.scala.md
[main/scala/api/commands/tblastn.scala]: commands/tblastn.scala.md
[main/scala/api/commands/blastx.scala]: commands/blastx.scala.md
[main/scala/api/commands/makeblastdb.scala]: commands/makeblastdb.scala.md