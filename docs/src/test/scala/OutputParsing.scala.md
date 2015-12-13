
```scala
package ohnosequences.blast.test

import ohnosequences.blast.api._
import ohnosequences.blast.api._, outputFields._
import ohnosequences.cosas._, types._, klists._, records._
import java.io.File

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
    qseqid  :×:
    qlen    :×:
    sseqid  :×:
    sgi     :×:
    sacc    :×:
    slen    :×:
    qstart  :×:
    qend    :×:
    sstart  :×:
    send    :×:
    outputFields.evalue :×: |[AnyOutputField]
  )

  test("can parse BLAST output") {

    import csvUtils._

    val blastOutput: File = new File("blastn.test3.out.txt")

    rows(blastOutput)(outRecord.keys.types map typeLabel asList) map { row => outRecord.parse(row) } foreach {

      optRec => optRec match {

        case Right(b) =>  {

          println("correctly parsed record:")
          // TODO map poly
          println( b.value map denotationValue )
        }

        case Left(v) => println{ s"oh, an error: ${v}" }
      }
    }
  }
}

```




[test/scala/CommandGeneration.scala]: CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: OutputFieldsSpecification.scala.md
[main/scala/api.scala]: ../../main/scala/api.scala.md
[main/scala/data.scala]: ../../main/scala/data.scala.md