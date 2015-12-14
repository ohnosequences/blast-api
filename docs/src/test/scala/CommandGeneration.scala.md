
```scala
package ohnosequences.blast.test

import ohnosequences.blast.api._
import ohnosequences.cosas._
import java.io.File

class CommandGeneration extends org.scalatest.FunSuite {

  val dbFile    = new File("/tmp/buh")
  val queryFile = new File("/tmp/query")
  val outFile   = new File("/tmp/blastout")

  test("command generation") {

    // val expr = BlastExpression(command)
    // val blastnCmd = blastn((
    //   blastn.arguments(
    //     db(dbFile)       :~:
    //     query(queryFile) :~:
    //     out(outFile)     :~: ∅
    //   ),
    //   blastn.defaults
    // ))
    //
    // assert {
    //   blastnCmd.cmd === Seq("blastn", "-db", "/tmp/buh", "-query", "/tmp/query", "-out", "/tmp/blastout") ++
    //     blastn.defaultsAsSeq
    // }
  }
}

```




[test/scala/CommandGeneration.scala]: CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: OutputFieldsSpecification.scala.md
[main/scala/api.scala]: ../../main/scala/api.scala.md
[main/scala/data.scala]: ../../main/scala/data.scala.md