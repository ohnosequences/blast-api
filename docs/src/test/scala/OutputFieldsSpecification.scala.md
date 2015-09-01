
```scala
package ohnosequences.blast.test

import ohnosequences.blast._, api._, data._, outputFields._
import ohnosequences.cosas._, typeSets._, properties._, records._
import java.io.File

class OutputFieldsSpecification extends org.scalatest.FunSuite {

  val dbFile    = new File("/tmp/buh")
  val queryFile = new File("/tmp/query")
  val outFile   = new File("/tmp/blastout")

  case object outRec extends BlastOutputRecord(qseqid :&: sseqid :&: □)

  case object exprType extends BlastExpressionType(blastn)(outRec)

  val stmt = BlastExpression(exprType)(
    argumentValues = blastn.arguments(
      db(dbFile)       :~:
      query(queryFile) :~:
      out(outFile)     :~: ∅
    ),
    optionValues = blastn.defaults update (num_threads(24) :~: ∅)
  )

  test("can build commands") {

    println(stmt.cmd)
  }

  test("can specify output data") {

    case object outputType extends BlastOutputType(exprType, "test.output")

    case object blastnOutput extends BlastOutput(outputType, "sample-blastn-output")

  }


}

```




[test/scala/CommandGeneration.scala]: CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: OutputFieldsSpecification.scala.md
[main/scala/api.scala]: ../../main/scala/api.scala.md
[main/scala/data.scala]: ../../main/scala/data.scala.md