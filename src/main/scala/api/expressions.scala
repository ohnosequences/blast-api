package ohnosequences.blast.api

import ohnosequences.cosas._, types._

/*
  This trait models a command part of the `BLAST` suite, like `blastn`, `blastp`, or `makeblastdb`.

  All the BLAST suite commands, together with their arguments, options and default values
  are defined in separate files in the (commands/)[commands/] folder (but in the same `api` namespace).
*/
// sealed
trait AnyBlastCommand {

  /* This label **should** match the **name** of the command */
  lazy val name: Seq[String] = Seq(this.toString)

  type Arguments <: AnyBlastOptionsRecord
  type Options   <: AnyBlastOptionsRecord

  type ArgumentsVals <: Arguments#Raw
  type OptionsVals   <: Options#Raw

  /* default values for options; they are *optional*, so should have default values. */
  val defaults: Options := OptionsVals

  /* valid output fields for this command */
  type ValidOutputFields <: AnyBlastOutputFields
}

/*
  An expression is a BLAST command with all the needed arguments given and ready to be executed
*/
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

  /* For command values we generate a `Seq[String]` which is valid command expression that you can
     execute (assuming BLAST installed) using `scala.sys.process` or anything similar. */
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
