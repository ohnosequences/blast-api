package ohnosequences.blast.test

import ohnosequences.blast.api.igblastn.output
import ohnosequences.blast.api.parse._, igblastn._
import ohnosequences.cosas._, types._, klists._, records._

case object format {}

class TCRAOutput extends org.scalatest.FunSuite {

  import output._, TCRA._

  lazy val lines =
    io.Source.fromFile("data/igblastn/tcra.out").getLines.toList

  test("parse hit table") {

    val headers =
      hitTable.keys.types map typeLabel asList

    val parsed =
      regionFrom(igblastn.hitTable, lines)
        .map(tabSeparatedFields)
        .map({ fields => groupFieldsWithHeaders(fields, headers) })
        .map({ hitTable.parse(_) })
  }

  test("parse VJ annotation") {

    val headers =
      vj_annotation.keys.types map typeLabel asList

    val parsed =
      regionFrom(vdjAnnotation, lines)
        .map(tabSeparatedFields)
        .map({ fields => groupFieldsWithHeaders(fields, headers) })
        .map({ vj_annotation.parse(_) })
  }

  test("parse VJ junction") {

    val headers =
      vj_junction.keys.types map typeLabel asList

    val parsed =
      regionFrom(vdjSequences, lines)
        .map(tabSeparatedFields)
        .map({ fields => groupFieldsWithHeaders(fields, headers) })
        .map({ vj_junction.parse(_) })
  }

  test("parse V annotation") {

    val headers =
      v_annotation.keys.types map typeLabel asList

    val parsed =
      regionFrom(vAnnotation, lines)
        .map(tabSeparatedFields)
        .map({ fields => groupFieldsWithHeaders(fields, headers) })
        .map({ v_annotation.parse(_) })
  }

  // TODO implement this; need to join regions with some logic
  ignore("parse CDR1 annotation") {}
  ignore("parse CDR2 annotation") {}
  ignore("parse CDR3 annotation") {}
}
