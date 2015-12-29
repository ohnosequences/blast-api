
```scala
package ohnosequences.blast.test

import ohnosequences.blast._, api._, data._, outputFields._
import ohnosequences.cosas._, types._, klists._, records._
import better.files._

class CommandGeneration extends org.scalatest.FunSuite {

  val dbFile    = File("/tmp/buh")
  val queryFile = File("/tmp/query")
  val outFile   = File("/tmp/blastout")

  case object outRec extends BlastOutputRecord(qseqid :×: sseqid :×: |[AnyOutputField])

  case object exprType extends BlastExpressionType(blastn)(outRec)

  val stmt = BlastExpression(exprType)(
    argumentValues = blastn.arguments(
      db(dbFile)       ::
      query(queryFile) ::
      out(outFile)     :: *[AnyDenotation]
    ),
    optionValues = blastn.defaults
    )

  test("command generation") {

    assert {
      stmt.cmd === Seq("blastn", "-db", "/tmp/buh", "-query", "/tmp/query", "-out", "/tmp/blastout") ++
        blastn.defaultsAsSeq ++ Seq("-outfmt", "10 qseqid sseqid")
    }
  }
}

```




[test/scala/CommandGeneration.scala]: CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: OutputFieldsSpecification.scala.md
[main/scala/api.scala]: ../../main/scala/api.scala.md
[main/scala/data.scala]: ../../main/scala/data.scala.md