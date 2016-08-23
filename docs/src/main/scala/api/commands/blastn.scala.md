
```scala
package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, klists._
import ohnosequences.blast.api.outputFields._

case object blastn extends AnyBlastCommand {

  type Arguments = arguments.type
  case object arguments extends RecordType(db :×: query :×: out :×: |[AnyBlastOption])

  type Options = options.type
  case object options extends RecordType(
    num_threads     :×:
    task            :×:
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
  )

  type ArgumentsVals =
    (db.type    := db.Raw)    ::
    (query.type := query.Raw) ::
    (out.type   := out.Raw)   ::
    *[AnyDenotation]

  type OptionsVals =
    (num_threads.type     := num_threads.Raw)     ::
    (task.type            := task.Raw)            ::
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
```

Default values match those documented in [the official BLAST docs](http://www.ncbi.nlm.nih.gov/books/NBK279675/) whenever possible.

```scala
  val defaults: Options := OptionsVals = options (
    num_threads(1)        ::
    task(blastn: Task)    ::
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

  type ValidOutputFields =
    qseqid.type     :×:
    sseqid.type     :×:
    sgi.type        :×:
    qstart.type     :×:
    qend.type       :×:
    sstart.type     :×:
    send.type       :×:
    qlen.type       :×:
    slen.type       :×:
    qseq.type       :×:
    sseq.type       :×:
    outputFields.evalue.type :×:
    bitscore.type   :×:
    score.type      :×:
    length.type     :×:
    pident.type     :×:
    mismatch.type   :×:
    positive.type   :×:
    gapopen.type    :×:
    gaps.type       :×:
    qcovs.type      :×:
    nident.type     :×:
    ppos.type       :×:
    |[AnyOutputField]

  // task depends on each command, that's why it is here.
  case object task extends BlastOption[Task](t => t.name) {
    def apply(t: Task): this.type := Task = this := t
  }
  sealed abstract class Task(val name: String)
  case object megablast       extends Task( "megablast" )
  case object dcMegablast     extends Task( "dc-megablast" )
  case object blastn          extends Task( "blastn" )
  case object blastnShort     extends Task( "blastn-short" )
  case object rmblastn        extends Task( "rmblastn" )

  def apply[R <: AnyBlastOutputRecord.For[this.type]](
    outputRecord: R,
    argumentValues: ArgumentsVals,
    optionValues: OptionsVals
  ): BlastExpression[this.type, R] = BlastExpression(this)(outputRecord, argumentValues, optionValues)
}

```




[test/scala/CommandGeneration.scala]: ../../../../test/scala/CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: ../../../../test/scala/OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: ../../../../test/scala/OutputFieldsSpecification.scala.md
[main/scala/api/outputFields.scala]: ../outputFields.scala.md
[main/scala/api/options.scala]: ../options.scala.md
[main/scala/api/package.scala]: ../package.scala.md
[main/scala/api/expressions.scala]: ../expressions.scala.md
[main/scala/api/commands/blastn.scala]: blastn.scala.md
[main/scala/api/commands/blastp.scala]: blastp.scala.md
[main/scala/api/commands/tblastx.scala]: tblastx.scala.md
[main/scala/api/commands/tblastn.scala]: tblastn.scala.md
[main/scala/api/commands/blastx.scala]: blastx.scala.md
[main/scala/api/commands/makeblastdb.scala]: makeblastdb.scala.md