package ohnosequences.blast.test

import ohnosequences.blast._, api._, outputFields._
import ohnosequences.cosas._, types._, klists._, records._
import better.files._

class CommandGeneration extends org.scalatest.FunSuite {

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
    optionValues =
      blastn.defaults.value
  )

  test("command generation") {

    assert {
      stmt.toSeq ===
        Seq("blastn", "-db", "/tmp/buh", "-query", "/tmp/query", "-out", "/tmp/blastout") ++
        blastn.defaults.value.toSeq ++
        Seq("-outfmt", "10 qseqid sseqid")
    }

    val mkdb = makeblastdb(
      argumentValues =
        in(file"refs.fasta")          ::
        input_type(DBInputType.fasta) ::
        dbtype(BlastDBType.nucl)      ::
        *[AnyDenotation],
      optionValues =
        (makeblastdb.defaults update title("refs.fasta")).value
    )

    import sys.process._

    mkdb.toSeq.!!
  }
}
