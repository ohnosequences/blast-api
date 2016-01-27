package ohnosequences.blast

import ohnosequences.cosas._, types._, records._, fns._, klists._, typeUnions._
import better.files._

package object api {

  type AnyBlastOptionsRecord = AnyRecordType { type Keys <: AnyProductType { type TypesBound <: AnyBlastOption } }

  implicit def blastOptionsOps[L <: AnyKList.withBound[AnyDenotation]](l: L):
    BlastOptionsOps[L] =
    BlastOptionsOps[L](l)

  /*
    Given a BLAST command, we can choose an output record made out of output fields. Each command specifies through its `ValidOutputFields` which fields can be used for it; this is checked when you construct a `BlastExpression`.
  */
  type AnyBlastOutputFields = AnyProductType { type TypesBound <: AnyOutputField }
  type AnyBlastOutputRecord = AnyRecordType { type Keys <: AnyBlastOutputFields }
  type BlastOutputRecord[OFs <: AnyBlastOutputFields] = RecordType[OFs]

  type isValidOutputRecordFor[R <: AnyBlastOutputRecord, C <: AnyBlastCommand] =
    R#Keys#Types#AllTypes isSubunionOf C#ValidOutputFields#Types#AllTypes

  def blastOutputRecordToSeq[R <: AnyBlastOutputRecord](rec: R): Seq[String] = {
    // '10' is the code for csv output
    val fieldsSeq: Seq[String] = "10" :: rec.keys.types.asList.map{ _.label }
    Seq("-outfmt", fieldsSeq.mkString(" "))
  }
}

case class BlastOptionsOps[L <: AnyKList.withBound[AnyDenotation]](val l: L) extends AnyVal {

  def toSeq(implicit opsToSeq: api.BlastOptionsToSeq[L]): Seq[String] = opsToSeq(l)
}
