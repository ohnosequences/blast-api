package ohnosequences.blast.test

import ohnosequences.blast._, api._, data._, outputFields._
import ohnosequences.cosas._, types._, klists._, records._
import better.files._

class OutputFieldsSpecification extends org.scalatest.FunSuite {

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
    optionValues = blastn.defaults update (num_threads(24) :: *[AnyDenotation])
  )

  test("can specify output data") {

    case object outputType extends BlastOutputType(exprType, "test.output")

    case object blastnOutput extends BlastOutput(outputType, "sample-blastn-output")
  }
}
