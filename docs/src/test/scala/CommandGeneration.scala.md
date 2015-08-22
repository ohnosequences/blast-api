
```scala
package ohnosequences.blast.test

import ohnosequences.blast.api._
import ohnosequences.cosas.typeSets._
import java.io.File

class CommandGeneration extends org.scalatest.FunSuite {

  val dbFile    = new File("/tmp/buh")
  val queryFile = new File("/tmp/query")
  val outFile   = new File("/tmp/blastout")

  test("command generation") {

    val blastnCmd = blastn((
      blastn.arguments(
        db(dbFile)       :~:
        query(queryFile) :~:
        out(outFile)     :~: ∅
      ),
      blastn.defaults
    ))

    assert {
      blastnCmd.cmd === Seq("blastn", "-db", "/tmp/buh", "-query", "/tmp/query", "-out", "/tmp/blastout") ++
        blastn.defaultsAsSeq
    }
  }
}

```




[test/scala/CommandGeneration.scala]: CommandGeneration.scala.md
[main/scala/blast.scala]: ../../main/scala/blast.scala.md