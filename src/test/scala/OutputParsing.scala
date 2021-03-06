package ohnosequences.blast.test

import ohnosequences.blast.api._, outputFields._
import ohnosequences.cosas._, types._
import java.io._

case object csvUtils {

  import com.github.tototoshi.csv._
  def csvReader(file: File): CSVReader = CSVReader.open(file)

  def lines(file: File): Iterator[Seq[String]] = csvReader(file) iterator

  // TODO much unsafe, add errors
  def rows(file: File)(headers: Seq[String]): Iterator[Map[String,String]] =
    lines(file) map { line => (headers zip line) toMap }
}

class ParseBlastOutput extends org.scalatest.FunSuite {

  case object outRecord extends BlastOutputRecord(
    qseqid              :×:
    qlen                :×:
    sseqid              :×:
    sgi                 :×:
    sacc                :×:
    slen                :×:
    qstart              :×:
    qend                :×:
    sstart              :×:
    send                :×:
    outputFields.evalue :×: |[AnyOutputField]
  )

  test("can parse BLAST output") {

    import csvUtils._

    val blastOutput: File = new File("blastn.test3.out.txt")

    val allOk =
      (rows(blastOutput)(outRecord.keys.types map typeLabel asList) map { row => outRecord.parse(row) }).foldLeft(true){
        (flag,optRec) =>
          optRec match {
            case Right(_) => flag
            case Left(_)  => false
      }
    }

    assert { allOk }
  }
}
