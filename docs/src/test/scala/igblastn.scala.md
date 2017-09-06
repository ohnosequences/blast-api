
```scala
package ohnosequences.blast.test

import ohnosequences.blast.api.igblastn.output
import ohnosequences.blast.api._
import ohnosequences.cosas._, types._, klists._, records._

class TCRAOutput extends org.scalatest.FunSuite {

  import output._, TCRA._

  lazy val lines =
    io.Source.fromFile("data/igblastn/tcra.out").getLines.toList

  def allRight[X,Y](s: Seq[Either[X,Y]]) = ! s.exists(_.isLeft)

  test("VJ Rearrangement summary") {

    assert { allRight(parse.igblastn.TCRA parseVJRearrangementSummary lines) }
  }

  test("VJ junction details") {

    assert { allRight(parse.igblastn.TCRA parseVJJunctionDetails lines) }
  }

  test("parse CDR3 sequence") {

    assert { allRight(parse.igblastn.TCRA parseCDR3Sequence lines) }
  }

  test("parse V Region annotations") {

    assert { allRight(parse.igblastn.TCRA parseVRegionAnnotations lines) }
  }
}

/////////////////////////////////////////////////////////////////////////////////////////

class TCRBOutput extends org.scalatest.FunSuite {

  import output._, TCRB._

  lazy val lines =
    io.Source.fromFile("data/igblastn/tcrb.out").getLines.toList

  def allRight[X,Y](s: Seq[Either[X,Y]]) = ! s.exists(_.isLeft)

  test("V(D)J Rearrangement summary") {

    assert { allRight(parse.igblastn.TCRB parseVDJRearrangementSummary lines) }
  }

  test("V(D)J junction details") {

    assert { allRight(parse.igblastn.TCRB parseVDJJunctionDetails lines) }
  }

  test("parse CDR3 sequence") {

    assert { allRight(parse.igblastn.TCRB parseCDR3Sequence lines) }
  }

  test("parse V Region annotations") {

    assert { allRight(parse.igblastn.TCRB parseVRegionAnnotations lines) }
  }

  test("parse hit table") {

    assert { allRight(parse.igblastn.TCRB parseHitTable lines) }
  }
}

```




[test/scala/CommandGeneration.scala]: CommandGeneration.scala.md
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