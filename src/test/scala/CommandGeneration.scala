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
