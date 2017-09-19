
```scala
package ohnosequences.blast.test

import ohnosequences.blast.api._, outputFields._
import ohnosequences.cosas._, types._, klists._, records._
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

```




[main/scala/api/commands/blastn.scala]: ../../main/scala/api/commands/blastn.scala.md
[main/scala/api/commands/blastp.scala]: ../../main/scala/api/commands/blastp.scala.md
[main/scala/api/commands/blastx.scala]: ../../main/scala/api/commands/blastx.scala.md
[main/scala/api/commands/igblastn.scala]: ../../main/scala/api/commands/igblastn.scala.md
[main/scala/api/commands/makeblastdb.scala]: ../../main/scala/api/commands/makeblastdb.scala.md
[main/scala/api/commands/tblastn.scala]: ../../main/scala/api/commands/tblastn.scala.md
[main/scala/api/commands/tblastx.scala]: ../../main/scala/api/commands/tblastx.scala.md
[main/scala/api/expressions.scala]: ../../main/scala/api/expressions.scala.md
[main/scala/api/options.scala]: ../../main/scala/api/options.scala.md
[main/scala/api/outputFields.scala]: ../../main/scala/api/outputFields.scala.md
[main/scala/api/package.scala]: ../../main/scala/api/package.scala.md
[main/scala/api/parse/igblastn.scala]: ../../main/scala/api/parse/igblastn.scala.md
[test/scala/CommandGeneration.scala]: CommandGeneration.scala.md
[test/scala/igblastn.scala]: igblastn.scala.md
[test/scala/igblastnClonotypesOutput.scala]: igblastnClonotypesOutput.scala.md
[test/scala/OutputFieldsSpecification.scala]: OutputFieldsSpecification.scala.md
[test/scala/OutputParsing.scala]: OutputParsing.scala.md