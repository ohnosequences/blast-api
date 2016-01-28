
```scala
package ohnosequences.blast.test

import ohnosequences.blast._, api._, outputFields._
import ohnosequences.cosas._, types._, klists._, records._
import better.files._

class OutputFieldsSpecification extends org.scalatest.FunSuite {

  val dbFile    = File("/tmp/buh")
  val queryFile = File("/tmp/query")
  val outFile   = File("/tmp/blastout")

  case object outRec extends BlastOutputRecord(qseqid :×: sseqid :×: |[AnyOutputField])

  val stmt = blastn(
    outRec,
    argumentValues =
      db(dbFile)       ::
      query(queryFile) ::
      out(outFile)     ::
      *[AnyDenotation],
    optionValues = blastn.defaults update (num_threads(24) :: *[AnyDenotation]) value
  )
}

```




[main/scala/api/commands/blastn.scala]: ../../main/scala/api/commands/blastn.scala.md
[main/scala/api/commands/blastp.scala]: ../../main/scala/api/commands/blastp.scala.md
[main/scala/api/commands/blastx.scala]: ../../main/scala/api/commands/blastx.scala.md
[main/scala/api/commands/makeblastdb.scala]: ../../main/scala/api/commands/makeblastdb.scala.md
[main/scala/api/commands/tblastn.scala]: ../../main/scala/api/commands/tblastn.scala.md
[main/scala/api/commands/tblastx.scala]: ../../main/scala/api/commands/tblastx.scala.md
[main/scala/api/expressions.scala]: ../../main/scala/api/expressions.scala.md
[main/scala/api/options.scala]: ../../main/scala/api/options.scala.md
[main/scala/api/outputFields.scala]: ../../main/scala/api/outputFields.scala.md
[main/scala/api/package.scala]: ../../main/scala/api/package.scala.md
[test/scala/CommandGeneration.scala]: CommandGeneration.scala.md
[test/scala/OutputFieldsSpecification.scala]: OutputFieldsSpecification.scala.md
[test/scala/OutputParsing.scala]: OutputParsing.scala.md