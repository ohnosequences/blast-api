
```scala
package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, klists._
import ohnosequences.blast.api.outputFields._

case object igblastn extends AnyBlastCommand {

  type Arguments = arguments.type
  case object arguments extends RecordType(
    query         :×:
    germline_db_V :×:
    germline_db_D :×:
    germline_db_J :×:
    ig_seqtype    :×:
    organism      :×:
    clonotype_out :×:
    out           :×:
    |[AnyBlastOption]
  )

  type Options = options.type
  case object options extends RecordType(
    num_threads         :×:
    evalue              :×:
    max_target_seqs     :×:
    strand              :×:
    word_size           :×:
    show_gis            :×:
    ungapped            :×:
    penalty             :×:
    reward              :×:
    perc_identity       :×:
    // IgBLAST-specific
    num_alignments_V    :×:
    num_alignments_D    :×:
    num_alignments_J    :×:
    min_V_length        :×:
    min_J_length        :×:
    min_D_match         :×:
    D_penalty           :×:
    extend_align5end    :×:
    num_clonotype       :×:
    show_translation    :×:
    domain_system       :×:
    focus_on_V_segment  :×:
    |[AnyBlastOption]
  )

  type ArgumentsVals =
    (query.type := query.Raw)                  ::
    (germline_db_V.type := germline_db_V.Raw)  ::
    (germline_db_D.type := germline_db_D.Raw)  ::
    (germline_db_J.type := germline_db_J.Raw)  ::
    (ig_seqtype.type := ig_seqtype.Raw)        ::
    (organism.type := organism.Raw)            ::
    (clonotype_out.type := clonotype_out.Raw)  ::
    (out.type := out.Raw)                      ::
    *[AnyDenotation]

  type OptionsVals =
    (num_threads.type         := num_threads.Raw)         ::
    (evalue.type              := evalue.Raw)              ::
    (max_target_seqs.type     := max_target_seqs.Raw)     ::
    (strand.type              := strand.Raw)              ::
    (word_size.type           := word_size.Raw)           ::
    (show_gis.type            := show_gis.Raw)            ::
    (ungapped.type            := ungapped.Raw)            ::
    (penalty.type             := penalty.Raw)             ::
    (reward.type              := reward.Raw)              ::
    (perc_identity.type       := perc_identity.Raw)       ::
    (num_alignments_V.type    := num_alignments_V.Raw)    ::
    (num_alignments_D.type    := num_alignments_D.Raw)    ::
    (num_alignments_J.type    := num_alignments_J.Raw)    ::
    (min_V_length.type        := min_V_length.Raw)        ::
    (min_J_length.type        := min_J_length.Raw)        ::
    (min_D_match.type         := min_D_match.Raw)         ::
    (D_penalty.type           := D_penalty.Raw)           ::
    (extend_align5end.type    := extend_align5end.Raw)    ::
    (num_clonotype.type       := num_clonotype.Raw)       ::
    (show_translation.type    := show_translation.Raw)    ::
    (domain_system.type       := domain_system.Raw)       ::
    (focus_on_V_segment.type  := focus_on_V_segment.Raw)  ::
    *[AnyDenotation]
```

Default values match those documented in [the official BLAST docs](http://www.ncbi.nlm.nih.gov/books/NBK279675/) whenever possible.

```scala
  val defaults: Options := OptionsVals = options (
    num_threads(1)                          ::
    evalue(BigDecimal(10))                  ::
    max_target_seqs(500)                    ::
    strand(Strands.both)                    ::
    word_size(11)                           ::
    show_gis(false)                         ::
    ungapped(false)                         ::
    penalty(-3)                             ::
    reward(2)                               ::
    perc_identity(0D)                       ::
    num_alignments_V(3)                     ::
    num_alignments_D(3)                     ::
    num_alignments_J(3)                     ::
    min_V_length(9)                         ::
    min_J_length(0)                         ::
    min_D_match(5)                          ::
    D_penalty(-4)                           ::
    extend_align5end(false)                 ::
    num_clonotype(100)                      ::
    show_translation(false)                 ::
    domain_system(IgBLASTDomainSystem.imgt) ::
    focus_on_V_segment(false)               ::
    *[AnyDenotation]
  )
```

`igblastn` does not support the csv output format

```scala
  type ValidOutputFields =
    |[AnyOutputField]

  case class Expression(argumentValues: ArgumentsVals, optionValues: OptionsVals) {

    lazy val argValsToSeq: BlastOptionsToSeq[ArgumentsVals] =
      implicitly[BlastOptionsToSeq[ArgumentsVals]]

    lazy val optValsToSeq: BlastOptionsToSeq[OptionsVals] =
      implicitly[BlastOptionsToSeq[OptionsVals]]

    def toSeq: Seq[String] =
      igblastn.name                 ++
      argValsToSeq(argumentValues)  ++
      optValsToSeq(optionValues)    ++
      Seq("-outfmt", "7")
  }

  def apply(argumentValues: ArgumentsVals, optionValues: OptionsVals): Expression =
    Expression(argumentValues, optionValues)
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
[main/scala/api/commands/igblastn.scala]: igblastn.scala.md