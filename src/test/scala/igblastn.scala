package ohnosequences.blast.test

import ohnosequences.blast.api.igblastn.output
import ohnosequences.blast.api.parse._, igblastn._
import ohnosequences.cosas._, types._, klists._, records._

case object format {}

class TCRAOutput extends org.scalatest.FunSuite {

  import output._, TCRA._

  lazy val lines =
    io.Source.fromFile("data/igblastn/tcra.out").getLines.toList

  def allRight[X,Y](s: Seq[Either[X,Y]]) = ! s.exists(_.isLeft)

  test("parse hit table") {

    val headers =
      HitTable.keys.types map typeLabel asList

    val parsed =
      regionFrom(hitTable, lines)
        .map(tabSeparatedFields)
        .map({ fields => groupFieldsWithHeaders(fields, headers) })
        .map({ HitTable.parse(_) })

    assert { allRight(parsed) }
  }

  test("parse VJ rearrangement summary") {

    val headers =
      VJRearrangementSummary.keys.types map typeLabel asList

    val parsed =
      regionFrom(vdjAnnotation, lines)
        .map(tabSeparatedFields)
        .map({ fields => groupFieldsWithHeaders(fields, headers) })
        .map({ VJRearrangementSummary.parse(_) })

    assert { allRight(parsed) }
  }

  test("parse CDR3 sequence") {

    val headers =
      CDR3Sequence.keys.types map typeLabel asList

    val parsed =
      regionFrom(cdr3Sequences, lines)
        .map(tabSeparatedFields).map(_.tail) // ugly I know
        .map({ fields => groupFieldsWithHeaders(fields, headers) })
        .map({ CDR3Sequence.parse(_) })

    assert { allRight(parsed) }
  }

  test("parse VJ junction details") {

    val headers =
      VJJunctionDetails.keys.types map typeLabel asList

    val parsed =
      regionFrom(vdjSequences, lines)
        .map(tabSeparatedFields)
        .map({ fields => groupFieldsWithHeaders(fields, headers) })
        .map({ VJJunctionDetails.parse(_) })

    assert { allRight(parsed) }
  }

  test("parse V region annotations") {

    val headers =
      VRegionAnnotations.keys.types map typeLabel asList

    val parsed =
      regionFrom(vAnnotation, lines)
        .map(tabSeparatedFields)
        .map({ fields => groupFieldsWithHeaders(fields, headers) })
        .map({ VRegionAnnotations.parse(_) })

    assert { allRight(parsed) }
  }
}
