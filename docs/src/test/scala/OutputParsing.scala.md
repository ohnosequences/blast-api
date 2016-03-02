
```scala
package ohnosequences.blast.test

import ohnosequences.blast.api._
import ohnosequences.blast.api._, outputFields._
import ohnosequences.cosas._, types._, klists._, records._
import better.files._

case object csvUtils {

  import com.github.tototoshi.csv._
  def csvReader(file: File): CSVReader = CSVReader.open(file.toJava)

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

    val blastOutput: File = File("blastn.test3.out.txt")

    rows(blastOutput)(outRecord.keys.types map typeLabel asList) map { row => outRecord.parse(row) } foreach { optRec =>

      optRec match {

        case Right(b) => {
          println("correctly parsed record:")
          println( b.value map denotationValue )
        }

        case Left(v)  => println{ s"oh, an error: ${v}" }
      }
    }
  }
}

```




[main/scala/api/commands/blastn.scala]: ../../main/scala/api/commands/blastn.scala.md
[main/scala/api/commands/blastp.scala]: ../../main/scala/api/commands/blastp.scala.md
[main/scala/api/commands/blastx.scala]: ../../main/scala/api/commands/blastx.scala.md
[main/scala/api/commands/makeblastdb.scala]: ../../main/scala/api/commands/makeblastdb.scala.md
[main/scala/api/commands/tblastn.scala]: ../../main/scala/api/commands/tblastn.scala.md
[main/scala/api/commands/tblastx.scala]: ../../main/scala/api/commands/tblastx.scala.md
[main/scala/api/expressions.scala]: ../../main/scala/api/expressions.scala.md
[main/scala/api/options.scala]: ../../main/scala/api/options.scala.md
[main/scala/api/outputFields.scala]: ../../main/scala/api/outputFields.scala.md
[main/scala/api/package.scala]: ../../main/scala/api/package.scala.md
[test/scala/CommandGeneration.scala]: CommandGeneration.scala.md
[test/scala/OutputFieldsSpecification.scala]: OutputFieldsSpecification.scala.md
[test/scala/OutputParsing.scala]: OutputParsing.scala.md