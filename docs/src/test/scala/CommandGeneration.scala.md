
```scala
package ohnosequences.blast.test

import ohnosequences.blast._, api._, outputFields._
import ohnosequences.cosas._, types._, klists._, records._
import java.io._

class CommandGeneration extends org.scalatest.FunSuite {

  val dbFiles   = Set(new File("/target/buh"), new File("/target/bah"))
  val queryFile = new File("/target/query")
  val outFile   = new File("/target/blastout")

  case object outRec extends BlastOutputRecord(qseqid :×: sseqid :×: |[AnyOutputField])

  val stmt = blastn(
    outRec,
    argumentValues =
      db(dbFiles)      ::
      query(queryFile) ::
      out(outFile)     ::
      *[AnyDenotation],
    optionValues =
      blastn.defaults.value
  )

  test("command generation") {

    assert {
      stmt.toSeq ===
        Seq("blastn", "-db", "/target/buh /target/bah", "-query", "/target/query", "-out", "/target/blastout") ++
        blastn.defaults.value.toSeq ++
        Seq("-outfmt", "10 qseqid sseqid")
    }
  }
}

```




[test/scala/CommandGeneration.scala]: CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: OutputFieldsSpecification.scala.md
[main/scala/api/outputFields.scala]: ../../main/scala/api/outputFields.scala.md
[main/scala/api/options.scala]: ../../main/scala/api/options.scala.md
[main/scala/api/package.scala]: ../../main/scala/api/package.scala.md
[main/scala/api/expressions.scala]: ../../main/scala/api/expressions.scala.md
[main/scala/api/parse/igblastn.scala]: ../../main/scala/api/parse/igblastn.scala.md
[main/scala/api/commands/blastn.scala]: ../../main/scala/api/commands/blastn.scala.md
[main/scala/api/commands/blastp.scala]: ../../main/scala/api/commands/blastp.scala.md
[main/scala/api/commands/tblastx.scala]: ../../main/scala/api/commands/tblastx.scala.md
[main/scala/api/commands/tblastn.scala]: ../../main/scala/api/commands/tblastn.scala.md
[main/scala/api/commands/blastx.scala]: ../../main/scala/api/commands/blastx.scala.md
[main/scala/api/commands/makeblastdb.scala]: ../../main/scala/api/commands/makeblastdb.scala.md
[main/scala/api/commands/igblastn.scala]: ../../main/scala/api/commands/igblastn.scala.md