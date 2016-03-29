package ohnosequences.blast.test

import ohnosequences.blast._, api._, outputFields._
import ohnosequences.cosas._, types._, klists._, records._
import better.files._

class OutputFieldsSpecification extends org.scalatest.FunSuite {

  val dbFiles   = Set(File("/tmp/buh"))
  val queryFile = File("/tmp/query")
  val outFile   = File("/tmp/blastout")

  case object outRec extends BlastOutputRecord(qseqid :×: sseqid :×: |[AnyOutputField])

  val stmt = blastn(
    outRec,
    argumentValues =
      db(dbFiles)      ::
      query(queryFile) ::
      out(outFile)     ::
      *[AnyDenotation],
    optionValues = blastn.defaults update (num_threads(24) :: *[AnyDenotation]) value
  )
}
