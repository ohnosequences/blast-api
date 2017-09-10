
```scala
package ohnosequences.blast.api.test

import ohnosequences.blast.api.parse.igblastn._, clonotypes._
import java.io.File
import java.nio.file._
import scala.collection.JavaConverters._

class IgBLASTOutput extends org.scalatest.FunSuite {

  def outputLines: Iterator[String] =
    Files.lines(new File("data/in/clonotype.out").toPath).iterator.asScala

  def summary: Iterator[Option[ClonotypeSummary]] =
    ClonotypeSummary parseFromLines outputLines

  def clonotypes: Iterator[Option[Clonotype]] =
    Clonotype parseFromLines outputLines

  test("Clonotypes summary") {

    summary foreach { opt => assert(opt.isDefined) }
  }

  test("Clonotypes detail") {

    clonotypes foreach { opt => assert(opt.isDefined) }
  }
}

```




[test/scala/CommandGeneration.scala]: CommandGeneration.scala.md
[test/scala/igblastnClonotypesOutput.scala]: igblastnClonotypesOutput.scala.md
[test/scala/OutputParsing.scala]: OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: OutputFieldsSpecification.scala.md
[test/scala/igblastn.scala]: igblastn.scala.md
[main/scala/api/outputFields.scala]: ../../main/scala/api/outputFields.scala.md
[main/scala/api/options.scala]: ../../main/scala/api/options.scala.md
[main/scala/api/package.scala]: ../../main/scala/api/package.scala.md
[main/scala/api/expressions.scala]: ../../main/scala/api/expressions.scala.md
[main/scala/api/parse/igblastn.scala]: ../../main/scala/api/parse/igblastn.scala.md
[main/scala/api/commands/blastn.scala]: ../../main/scala/api/commands/blastn.scala.md
[main/scala/api/commands/blastp.scala]: ../../main/scala/api/commands/blastp.scala.md
[main/scala/api/commands/tblastx.scala]: ../../main/scala/api/commands/tblastx.scala.md
[main/scala/api/commands/tblastn.scala]: ../../main/scala/api/commands/tblastn.scala.md
[main/scala/api/commands/blastx.scala]: ../../main/scala/api/commands/blastx.scala.md
[main/scala/api/commands/makeblastdb.scala]: ../../main/scala/api/commands/makeblastdb.scala.md
[main/scala/api/commands/igblastn.scala]: ../../main/scala/api/commands/igblastn.scala.md